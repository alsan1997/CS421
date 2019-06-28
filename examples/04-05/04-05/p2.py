# Objects!

def mkPoint(x):

    def getX():
        nonlocal x
        return x[0]

    def setX(nuX):
        nonlocal x
        oldX = x[0]
        x[0] = nuX
        return oldX

    def moveX(delta):
        nonlocal x
        oldX = x[0]
        x[0] = x[0] + delta
        return oldX

    return { "getX": getX
           , "setX": setX
           , "moveX": moveX
    }

def mkFastPoint(x):
    p = mkPoint(x)

    def moveX(delta):
        nonlocal x
        oldX = x[0]
        x[0] = x[0] + delta * 2
        return oldX

    p['moveX'] = moveX

    return p
