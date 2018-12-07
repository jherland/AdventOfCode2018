#!/usr/bin/env python

from parse import parse


def pairs():
    with open("07.input") as f:
        for line in f:
            a, b = parse(
                'Step {} must be finished before step {} can begin.', line)
            yield (a, b)


class Jobs:
    def __init__(self, pairs):
        self.d = {}
        for a, b in pairs:
            self.d.setdefault(a, set())
            self.d.setdefault(b, set()).add(a)

    def __len__(self):
        return len(self.d)

    def ready(self):
        '''Jobs that have no pending dependencies.'''
        for k, v in self.d.items():
            if not v:
                yield k

    def start(self, job):
        '''Start working on a job. It will be removed from the ready set.'''
        assert job in self.d
        del self.d[job]
        return job

    def finish(self, job):
        '''Finish working on a job. It will be removed from others' deps.'''
        assert job not in self.d
        for deps in self.d.values():
            deps.discard(job)


class Crew:
    def __init__(self, size, jobs):
        self.t = 0
        self.workers = [None] * size
        self.jobs = jobs
        self.done = []

    def __str__(self):
        workset = ' '.join(w[0] if w else '.' for w in self.workers)
        return "{:3}: {} | {}".format(self.t, workset, ''.join(self.done))

    def job_time(self, job):
        return 60 + 1 + ord(job) - ord('A')

    def handoff(self):
        for i, w in enumerate(self.workers):
            if w is None:
                continue
            job, remain = w
            if remain == 0:
                self.workers[i] = None
                self.jobs.finish(job)
                self.done.append(job)

    def pickup(self):
        ready = sorted(self.jobs.ready())
        for i, w in enumerate(self.workers):
            if not ready:
                break
            if w is None:
                job = ready.pop(0)
                remain = self.job_time(job)
                self.workers[i] = (job, remain)
                self.jobs.start(job)

    def tick(self):
        self.t += 1
        for i, w in enumerate(self.workers):
            if w is None:
                continue
            job, remain = w
            self.workers[i] = (job, remain - 1)

    def step(self):
        self.pickup()
        self.tick()
        print(self)
        self.handoff()

    def run(self):
        while self.jobs or any(self.workers):
            self.step()
        return self.t, self.done


if __name__ == '__main__':
    # part 1
    jobs = Jobs(pairs())
    while jobs:
        job = sorted(jobs.ready())[0]
        jobs.start(job)
        jobs.finish(job)
        print(job, end='')
    print()

    # part 2
    crew = Crew(5, Jobs(pairs()))
    t, done = crew.run()
    print(t, ''.join(done))
