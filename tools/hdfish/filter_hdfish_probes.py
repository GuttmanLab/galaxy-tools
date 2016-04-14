import argparse

parser = argparse.ArgumentParser(description = "Finds probes contained by "
                                               "intervals defined in a BED "
                                               "file. Prints valid probe "
                                               "records to stdout.")
parser.add_argument("-b", "--bed", required = True,
                    help = "The BED file containing the intervals of interest")
parser.add_argument("-p", "--probes", required = True,
                    help = "The file containing the probes to be searched")
args = parser.parse_args()

# Simple class to represent intervals spanned by BED records or probes
class Interval:

    def __init__(self, c, s, e):
        self.chrom = c
        self.start = s
        self.end = e

    def contains(self, other):
        return self.chrom == other.chrom and \
            self.start <= other.start and \
            self.end >= other.end

# Populate beds[] with BED records
beds = []

fBed = open(args.bed, 'r')
for line in fBed:
    fields = line.rstrip().split("\t")
    if len(fields) < 3 or fields[1] >= fields[2]:
        raise ValueException("Invalid BED record: " + line)
    beds.append(Interval(fields[0], int(fields[1]), int(fields[2])))
fBed.close()

# Filter and output probes contained in any bed in beds[]
fProbes = open(args.probes, 'r')
for line in fProbes:
    fields = line.rstrip().split("\t")
    if len(fields) != 5 or fields[3] >= fields[4]:
        raise ValueException("Invalid probe record: " + line)
    for b in beds:
        if b.contains(Interval(fields[2], int(fields[3]), int(fields[4]))):
            print(line.rstrip())
            break
fProbes.close()
