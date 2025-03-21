import csv
import random


def make_part_data(reader, part_id, output):
    for row in reader:
        for i,word in enumerate(row["sentence"].split()): #split by space
            region = 'not-disambig' # region
            if 'disambig' in row:
                if i >= int(row['disambig']) and i < int(row['disambig'])+2:
                    region = 'disambig'

            ambig = False # condition
            condition = row.get('condition', 'filler')
            if condition == 'ambiguous':
                ambig = True

            if condition == 'filler':
                sent_type = 'filler'
            else:
                sent_type = 'critical'


            if ambig and region == 'disambig':
                RT = random.randint(300, 500)
            else:
                RT = random.randint(150, 350)

            output.append([part_id, row["sentid"], condition, row["sentence"], word, i, RT, region])


def make_all_data(fpaths, num_parts):
    output = [['partid','sentid', 'condition', 'sentence', 'word', 'word_pos', 'RT', 'region']]
    for i in range(num_parts):
        for in_fpath in fpaths:
            with open(in_fpath, 'r') as f:
                stim_reader = csv.DictReader(f, delimiter='\t')
                make_part_data(stim_reader, i, output)

    out_fpath = '../data/empirical/toy_data.tsv'
    with open(out_fpath, 'w') as f:
        writer = csv.writer(f, delimiter='\t')
        writer.writerows(output)




fpaths = ['../stimuli/filler.tsv',
          '../stimuli/critical.tsv']


make_all_data(fpaths, 5)




