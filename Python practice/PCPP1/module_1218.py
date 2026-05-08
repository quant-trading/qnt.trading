import random

class Apple:
    
    apple_cntr = 0
    total_w = 0
    
    def __init__(self, w):
        Apple.apple_cntr = Apple.apple_cntr + 1
        self.w = w
        Apple.total_w = Apple.total_w + w



while Apple.apple_cntr < 1000 and Apple.total_w < 300:
    w = random.uniform(0.2, 0.5)
    if(Apple.total_w + w < 300):
        apple = Apple(w)
    else:
        break
    

print(Apple.apple_cntr)
print(Apple.total_w)