import cv2
import numpy as np
import math

def less_then(a,b):
  if not (a == None or b == None):
    return a.x  < b.x - 0.5        # this is to keep dist > 0.5       
  else:
    return True
def filter_less_then(a,b):
  if not (a == None or b == None):
    return a.x  < b.x       
  else:
    return True
def opposite(move):
  lr = ['move_left', 'move_right']
  ud = ['move_up', 'move_down']
  if move in lr:
    lr.remove(move)
    return lr[0]
  if move in ud:
    ud.remove(move)
    return ud[0]
def color(heavy):
  if not heavy:
    return (0,225,0)
  else:
    return (0,0,225)
    
def text(shape):
  if shape == 'square':
    return 'S'
  elif shape == 'triangle':
    return 'T'
  elif shape == 'circle':
    return 'C'
  else:
    return None
def draw_image(world):
  font = cv2.FONT_HERSHEY_SIMPLEX
  fontScale = 0.5
  fontColor = (0,0,0)
  lineType = 2
  img = np.zeros((500,500,3),np.uint8)
  img.fill(255)
  for obj in world.objects:
    img = cv2.circle(img, (int(obj.x*100),int(obj.y*100)),int(obj.r*100) ,color(obj.shape),-1)
    cv2.putText(img, text(obj.shape), (int(obj.x*100),int(obj.y*100)), font, fontScale, fontColor, lineType)
  img = cv2.flip(img,0)
  cv2.imshow('World {}'.format(world.id),img)
  cv2.waitKey()
 
def diffX(n,p):
  return "displX({})".format(round(n.x-p.x,1))
def diffY(n,p):
  return "displY({})".format(round(n.y-p.y,1))
  
def draw_obj(img, obj):
  if obj.shape == 'square':
    return draw_square(img, obj)
  elif obj.shape == 'triangle':
    return draw_triangle(img, obj)
  elif obj.shape == 'circle':
    return draw_circle(img, obj)
  else:
    return None  
  
def draw_square(img, obj):
  a = math.sqrt((obj.r**2)/2)
  start_point = (int((obj.x-a) * 100), int(500 - (obj.y+a)*100))
  end_point = (int((obj.x+a) * 100), int(500 - (obj.y-a)*100))
  return cv2.rectangle(img, start_point, end_point, color(obj.heavy), -1) 

def draw_circle(img, obj):
  return cv2.circle(img, (int(obj.x*100),int(500 - obj.y*100)),int(obj.r*100) ,color(obj.heavy),-1)

def draw_triangle(img, obj):
  sin = 0.5
  cos = math.sqrt(3)/2
  a = (int((obj.x - obj.r*cos)*100),int(500 - (obj.y - obj.r*sin)*100))
  b = (int((obj.x + obj.r*cos)*100),int(500 - (obj.y - obj.r*sin)*100))
  c = (int(obj.x*100),int(500 - (obj.y + obj.r)*100))  
  triangle_cnt = np.array([a,b,c])
  return cv2.drawContours(img, [triangle_cnt], 0, color(obj.heavy), -1)
  
def draw_state(exmp,prev=None):
  font = cv2.FONT_HERSHEY_SIMPLEX
  fontScale = 0.5
  fontColor = (0,0,0)
  lineType = 2

  img = np.zeros((500,500,3),np.uint8)
  img.fill(255)
  
  '''if prev is None:
    for obj in world.objects:
      img = draw_obj(img,obj)
      cv2.putText(img, "{}:".format(obj.id), (int(obj.x*100),int(500 - obj.y*100)), font, fontScale, fontColor, lineType)
  else:
    for objn,objp in zip(world.objects,prev.objects):
      img = draw_obj(img,objn)
      cv2.putText(img, "{}:".format(objn.id)+diff(objn,objp), (int(objn.x*100),int(500 - objn.y*100)), font, fontScale, fontColor, lineType)'''
  if prev is None:
    world = exmp.init_state
    for obj in world.objects:
      img = draw_obj(img,obj)
      #if obj.id == exmp.hier[1] or obj.id == exmp.hier[2]:
      #cv2.putText(img, "{},pos({})".format(obj.id,round(obj.x,1)), (int(obj.x*100),int(500 - obj.y*100)), font, fontScale, fontColor, lineType)
      #else:
      cv2.putText(img, "{}".format(obj.id), (int(obj.x*100),int(500 - obj.y*100)), font, fontScale, fontColor, lineType)
  else:
    world = exmp.next_state
    prev = exmp.init_state
    for objn,objp in zip(world.objects,prev.objects):
      img = draw_obj(img,objn)
      #print(objn.id,diff(objn,objp))
      #cv2.putText(img, "{}".format(objn.id), (int(objn.x*100),int(500 - objn.y*100)), font, fontScale, fontColor, lineType)
      if objn.x == objp.x and objn.y == objp.y:
        cv2.putText(img, "{}".format(objn.id), (int(objn.x*100),int(500 - objn.y*100)), font, fontScale, fontColor, lineType)
      else:
        if objn.x == objp.x:
          cv2.putText(img, "{},".format(objn.id)+diffY(objn,objp), (int(objn.x*100),int(500 - objn.y*100)), font, fontScale, fontColor, lineType)
        else:
          cv2.putText(img, "{},".format(objn.id)+diffX(objn,objp), (int(objn.x*100),int(500 - objn.y*100)), font, fontScale, fontColor, lineType)
  return img    
      
      
  return img
  
def draw_example(example):
  init = draw_state(example.init_state,None)
  div = img = np.zeros((500,2,3),np.uint8)
  next = draw_state(example.next_state,example.init_state)
  stacked_img = np.hstack((init, div))
  stacked_img = np.hstack((stacked_img, next))
  cv2.imshow("World {} | {} | {}".format(example.init_state.id, example.action.to_string(), example.format_hier()), stacked_img)
  cv2.waitKey()
def draw_example1(example):
  init = draw_state(example,None)
  div = np.zeros((500,2,3),np.uint8)
  next = draw_state(example,example)
  stacked_img = np.hstack((init, div))
  stacked_img = np.hstack((stacked_img, next))
  return stacked_img
  #cv2.imshow("World {} | {} | {}".format(example.init_state.id, example.action.to_string(), example.format_hier()), stacked_img)
  #cv2.waitKey()
  
def draw_example_new(example,X,Y=None):
  truth = draw_example1(example)
  '''init_state = example.init_state.copy()
  div = np.zeros((2,1002,3),np.uint8)
  for i, x in enumerate(X):
    print(i,"X")
    example.next_state = init_state.copy()
    example.next_state.objects[i].x = x
    prediction = draw_example1(example)
    stacked_img = np.vstack((truth,div))
    stacked_img = np.vstack((stacked_img,prediction))
    
    scale_percent = 80 # percent of original size
    width = int(stacked_img.shape[1] * scale_percent / 100)
    height = int(stacked_img.shape[0] * scale_percent / 100)
    dim = (width, height)
    # resize image
    resized = cv2.resize(stacked_img, dim, interpolation = cv2.INTER_AREA) 
    
    
    cv2.imshow("World {} | {} | {}".format(example.init_state.id, example.action.to_string(), example.format_hier()), resized)
    cv2.moveWindow("World {} | {} | {}".format(example.init_state.id, example.action.to_string(), example.format_hier()),0,0)
    cv2.waitKey(0)
    cv2.destroyAllWindows()
    
  if Y is not None:
    for i, y in enumerate(Y):
      print(i,"Y")
      example.next_state = init_state.copy()
      example.next_state.objects[i].y = y
      prediction = draw_example1(example)
      stacked_img = np.vstack((truth,div))
      stacked_img = np.vstack((stacked_img,prediction))
      
      scale_percent = 80 # percent of original size
      width = int(stacked_img.shape[1] * scale_percent / 100)
      height = int(stacked_img.shape[0] * scale_percent / 100)
      dim = (width, height)
      # resize image
      resized = cv2.resize(stacked_img, dim, interpolation = cv2.INTER_AREA)''' 
      
  
  cv2.imshow("World {} | {} | {}".format(example.init_state.id, example.action.to_string(), example.format_hier()), truth) #resized
  cv2.moveWindow("World {} | {} | {}".format(example.init_state.id, example.action.to_string(), example.format_hier()),0,0)
  cv2.waitKey(0)
  cv2.destroyAllWindows()
  
  
  
