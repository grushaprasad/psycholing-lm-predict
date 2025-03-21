import csv
import argparse
import os
import re
import numpy as np

## Note: this needs to be modified if I want to use a different punctuation. Then I want the puncutation to be on separate line ? 
## Or could say ignore punctuation then don't use that in length. Not sure it makes sense to treat punctuation as being separate or part of next word for this...

## make this take one path with stimuli and do this for all tsv files. 

parser = argparse.ArgumentParser()
parser.add_argument("--freqs_fpath", type=str)
parser.add_argument("--stim_fdir", type=str)
parser.add_argument("--result_fdir", type=str)

args = parser.parse_args()

with open(args.freqs_fpath, 'r') as f:
    reader = csv.DictReader(f)
    freqs = {}
    for row in reader:
        freqs[row["word"]] = int(row["count"])

def make_data(reader):
    output = [['sentid', 'item', 'sentence', 'word', 'word_pos', 'logfreq', 'length']]

    for row in stim_reader: 
        for i,word in enumerate(row["sentence"].split()): #split by space
            freq = freqs.get(re.sub("[.,?!;:]", "", word.lower()), 0)
            if freq > 0:
                logfreq = np.log(freq)
            else:
                logfreq = 'NA' ## will become NA in R

            length = len(word) ## will include punctuation

            itemid = row["sentid"]
            if 'item' in row:
                itemid = row["item"]

            output.append([row["sentid"], itemid, row["sentence"], word, i, logfreq, length])

    return output


files = os.listdir(args.stim_fdir)

for file in files:
    if file.split(".")[-1] != "tsv": continue
    print(file)
    with open(f"{args.stim_fdir}/{file}", 'r') as f:
        stim_reader = csv.DictReader(f, delimiter='\t')
        output = make_data(stim_reader)

    out_fpath = f"{args.result_fdir}/{file.split('/')[-1]}"
    with open(out_fpath, 'w') as f:
        writer = csv.writer(f, delimiter='\t')
        writer.writerows(output)

# with open(args.stim_fpath, 'r') as f:
#     stim_reader = csv.DictReader(f, delimiter='\t')

#     output = [['sentid', 'sentence', 'word', 'word_pos', 'logfreq', 'length']]

#     for row in stim_reader: 
#         for i,word in enumerate(row["sentence"].split()): #split by space
#             freq = freqs.get(re.sub("[.,?!;:]", "", word.lower()), 0)
#             if freq > 0:
#                 logfreq = np.log(freq)
#             else:
#                 logfreq = 'NA' ## will become NA in R

#             length = len(word) ## will include punctuation

#             output.append([row["sentid"], row["sentence"], word, i, logfreq, length])


# with open(args.result_fpath, 'w') as f:
#     writer = csv.writer(f, delimiter='\t')
#     writer.writerows(output)