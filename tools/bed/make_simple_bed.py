import argparse

def make_bed():
    parser = argparse.ArgumentParser(description="Generate a BED file")
    parser.add_argument("--bed", action="append")
    args = parser.parse_args();

    for b in args.bed:
        print(b)

if __name__ == "__main__":
    make_bed()
