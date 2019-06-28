# Objects!

def mkPoint(x,y):
    def getX():
        nonlocal x
        return x

    def setX(nuX):
        nonlocal x
        oldX = x
        x = nuX
        return oldX

    def moveX(delta):
        nonlocal x
        oldX = x
        x = x + delta
        return oldX

    return { "getX": getX
           , "setX" : setX
           , "moveX" : moveX
    }

def mkFastPoint(x,y):

    def moveX(ht):
        def doit(delta):
            nonlocal ht
            ht['setX'](ht['getX']() + delta * 2)
            return ht['x']
        return doit

    obj = mkPoint(x,y)
    obj["moveX"] = moveX(obj)

    return obj


