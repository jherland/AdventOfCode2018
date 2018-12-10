#!/usr/bin/env python3

from collections import Counter


def parse_timestamp(s):
    assert s[4] == '-' and s[7] == '-' and s[10] == ' ' and s[13] == ':'
    year, month, day, hour, minute = \
        map(int, [s[0:4], s[5:7], s[8:10], s[11:13], s[14:16]])
    return minute if hour == 0 else 0


def most_sleepy_minute(asleep_minutes):
    return Counter(asleep_minutes).most_common(1)[0]


def parse():
    with open("04.input") as f:
        guard = None
        asleep = False
        then = None
        for line in sorted(f.readlines()):
            line = line.strip()
            assert line[0] == '[' and line[17:19] == '] '
            now = parse_timestamp(line[1:17])
            event = line[19:]
            if event.startswith('Guard #') and event.endswith(' begins shift'):
                guard = int(event[7:-13])
                assert not asleep
            elif event == 'falls asleep':
                assert guard is not None
                assert not asleep
                asleep = True
                then = now
            elif event == 'wakes up':
                assert guard is not None
                assert asleep
                asleep = False
                for minute in range(then, now):
                    yield guard, minute
            else:
                assert False


if __name__ == '__main__':
    # When are guards asleep? map guard -> [minute]
    by_guard = {}
    for guard, minute in parse():
        by_guard.setdefault(guard, []).append(minute)

    # part 1
    sloth, minutes = max(by_guard.items(), key=lambda item: len(item[1]))
    print(sloth * most_sleepy_minute(minutes)[0])

    # part 2
    most_sleepy_by_guard = {
        g: most_sleepy_minute(mins) for g, mins in by_guard.items()}
    sloth2, (minute, asleep) = max(
        most_sleepy_by_guard.items(), key=lambda item: item[1][1])
    print(sloth2 * minute)
