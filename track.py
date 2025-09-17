import sqlite3
import argparse
from lines import Lines

def materialize_db():
    sq = sqlite3.connect('tracks.db')
    cursor = sq.execute('create table if not exists tracks (track varchar(255) not null, line integer not null, adj integer not null, primary key (track, line))')
    sq.commit()
    return sq

def add_entry(track, line, adj):
    with materialize_db() as sq:
        cursor = sq.execute('insert or replace into tracks (track, line, adj) values (?, ?, ifnull((select adj from tracks as t1 where t1.track = ? and t1.line < ? order by t1.line desc limit 1), 0) + ?)', (track, line, track, line, adj))
        sq.commit()

# Get 1024 more lines after the given line number.
def get_track_lines(lines, track, line, depth, limit = 1024):
    with materialize_db() as sq:
        cursor = sq.execute('select line, adj from tracks where track = ? and line < ? order by line desc limit 1', (track, line))
        prev_line = cursor.fetchone()
        cursor = sq.execute('select line, adj from tracks where track = ? and line >= ? order by line asc limit 1024', (track, line))
        line_to_depth_map = dict(cursor.fetchall())

        result = []

        if prev_line is not None:
            d = prev_line[1]
        else:
            d = 0

        while len(result) < limit and line < (lines.linedata_length / 8):
            d = line_to_depth_map.get(line, d)
            if d <= depth:
                lt = lines.getline(line)

                # Skip if we're at depth.
                if lt.startswith('cpu0:') and d == depth:
                    line += 1
                    continue

                result.append(line)

            line += 1

        return result

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('txt')
    parser.add_argument('lines')

    args = parser.parse_args()

    lines = Lines(args.txt, args.lines)
    for line in range(lines.length()):
        if line % 100000 == 0:
            print(line,'...')
        text = lines.getline(line)
        if text.startswith('00015cbc:'):
            print(line, text)
            add_entry('curtime_ppc', line + 1, 3)
        elif text.startswith('00015cc0:'):
            print(line, text)
            add_entry('curtime_ppc', line, -3)

