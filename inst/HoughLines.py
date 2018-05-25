import cv2
import numpy as np

def calcHoughLines(file):
  img = cv2.imread(file)
  gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
  edges = cv2.Canny(gray,50,150,apertureSize = 3)

  lines = cv2.HoughLines(edges,1,np.pi/180,15)
  return lines
