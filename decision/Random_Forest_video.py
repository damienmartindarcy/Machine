import cv2
import numpy as np

i = 0

def fill_blacks(image):
    height = image.shape[0]
    width  = image.shape[1]
    fill_blacks = image.copy()
    window = 5
    heightlim = np.int64(-1+height/window)
    widthlim  = np.int64(-1+width/window)

    print(heightlim)
    print(widthlim)

    
    
    for i in range(heightlim):
        for j in range(widthlim):
            submatrix = image[(i*window):(i+1)*window,(j*window):(j+1)*(window)]
            sum_total =  np.sum(submatrix)/(100)
            if (sum_total >10):
                fill_blacks[(i*window):(i+1)*window,(j*window):(j+1)*(window)] = 255
    return fill_blacks

#########################################


import pandas
from sklearn import tree
import pandas
from sklearn.ensemble import RandomForestClassifier
import numpy as np
import Matrix_CV_ML as ML
from sklearn import cross_validation

g = ML.Matrix_CV_ML("C:/Udemy/Practical Data Science in Python/Ex1",100,133)
g.build_ML_matrix()


clh = RandomForestClassifier(max_depth  = 3)
clh.fit(g.global_matrix, g.labels)
print(np.mean(cross_validation.cross_val_score(clh, g.global_matrix, g.labels, cv=25)))



##########################################


cap = cv2.VideoCapture(0)
count = 0


while(True):
    ret, frame = cap.read()
    hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
    lower_blue = np.array([0, 10, 60])
    upper_blue = np.array([20, 150, 255])
    
    mask = cv2.inRange(hsv, lower_blue, upper_blue)
    res  = cv2.bitwise_and(frame,frame, mask= mask)
    
    #name = "C:/Udemy/Practical Data Science in Python/Ex1/hand%d_4.jpg"%count
    ref  = fill_blacks(mask)
    #cv2.imwrite(name, ref)

    
    
    cv2.putText(frame,clh.predict(ref),(1,250), cv2.FONT_HERSHEY_SIMPLEX, 2,(255,255,255),2,cv2.LINE_AA)
    cv2.imshow('res',frame)       

    count = count + 1
    
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break


cap.release()
cv2.destroyAllWindows()
