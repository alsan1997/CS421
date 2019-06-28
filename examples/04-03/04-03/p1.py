def mkCounter():
    ct = 0
    def run():
        nonlocal ct
        ct = ct + 1
        return ct

    return run

class Delay:
    def __init__(self,action):
        self.action = action
        self.status = 0

    def report(self,x):
        print("Thunk executed: {}".format(x))
        return x

    def force(self):
        if self.status == 2:
            return self.value
        else:
            self.status = 1
            self.value = self.report(self.action())
            self.status = 2
            return self.value

def inPi():
    return 1 + 2.14

def lazyTake(n,x):
    if x == () or n < 1:
        return ()
    else:
        return (x[0],lazyTake(n-1,x[1].force()))

i1 = (2,Delay(lambda: (3, Delay(lambda: (5, Delay(lambda: (8, Delay(lambda: ()))))))))


def lazyMap(f,xx):
    if xx == ():
        return ()
    else:
        return (f(xx[0]), Delay(lambda: lazyMap(f,xx[1].force()) ))

def zipWith(f,xx,yy):
    if xx == () or yy == ():
        return ()
    else:
        return (f(xx[0],yy[0]), Delay(lambda: zipWith(f,xx[1].force(),yy[1].force()) ))

def inc(i):
    return i + 1

def plus(x,y):
    return x + y

def tail(xx):
    return xx[1].force()

nats = (1,Delay(lambda: lazyMap(inc,nats)))

fibs = (0,Delay(lambda: (1, Delay(lambda: zipWith(plus,fibs,tail(fibs))))))
