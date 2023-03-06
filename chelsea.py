
"""
Chelsea Hyland
CSCI - 112
lab 07 - sorting

file created March 2, 2023 by Chelsea Hyland
"""

## MERGE SORT
def mergeSort(alist):
    ## there is a problem with the last digit
    if len(alist) == 2:
        #check the order
        if alist[1] < alist[0]:
            print(alist)
            temp = alist[0]
            alist[0] = alist[1]
            alist[1] = temp
            print(alist)
   
    elif len(alist) > 2:
        third = len(alist)//3 # splits the list into thirds
        lefthalf = alist[:third]
        middle = alist[third: (third+third)]
        righthalf = alist[third+third:]

        print('left', lefthalf)
        print('middle',middle)
        print('right',righthalf)
       

        # recursive call on all three thirds
        mergeSort(lefthalf)
        mergeSort(middle)
        mergeSort(righthalf)

        print('aleft', lefthalf)
        print('amiddle',middle)
        print('aright',righthalf)
 

        h=0
        i=0
        j=0
        k=0

        first = ''
        while i < len(lefthalf) and j < len(righthalf) and h < len(middle):
            #if the left half is the smallest
            if lefthalf[i] <= righthalf[j] and lefthalf[i] <= middle[h]:
                alist[k]=lefthalf[i]
                i=i+1
                first = 'l'
               
            #if the right half is the smallest
            elif righthalf[j] <= lefthalf[i] and righthalf[j] <= middle[h]:
                alist[k]=righthalf[j]
                j=j+1
                first = 'r'
               
            #if the middle is the smallest
            elif middle[h] <= lefthalf[i] and middle[h] <= righthalf[j]:
                alist[k]=middle[h]
                h=h+1
                first = 'm'
               
            k=k+1

           
        # deals with the second and third digits if middle is the smallest
        #while i < len(lefthalf) and j < len(righthalf):
        if first == 'm':
            if lefthalf[i] <= righthalf[j]:
                alist[k]=lefthalf[i]
                i=i+1
                k=k+1
                alist[k]=righthalf[j]
                j=j+1
            elif righthalf[j] <= lefthalf[i]:
                alist[k]=righthalf[j]
                j=j+1
                k=k+1
                alist[k]=lefthalf[i]
                i=i+1
               
        # deals with the second and third digits if left half is the smallest
        #while j < len(righthalf) and h < len(middle):
        if first == 'l':
            #print('2')
            #print('alist', alist)
            if righthalf[j] <= middle[h]:
                alist[k]=righthalf[j]
                j=j+1
                k=k+1
                alist[k]=middle[h]
                h=h+1
                #print('alist', alist)
            elif middle[h] <= righthalf[j]:
                alist[k]=middle[h]
                h=h+1
                k=k+1
                alist[k]=righthalf[j]
                j=j+1


        # deals with the second and third digits if right half is the smallest
        #while i < len(lefthalf) and h < len(middle):
        if first == 'r':
            #print('3')
            if lefthalf[i] <= middle[h]:
                alist[k]=lefthalf[i]
                i=i+1
                k=k+1
                alist[k]=middle[h]
                h=h+1
                #print('alist', alist)
            elif middle[h] <= lefthalf[i]:
                alist[k]=middle[h]
                h=h+1
                k=k+1
                alist[k]=lefthalf[i]
                i=i+1
               
           


## QUICK SORT
def quickSort(alist):
   quickSortHelper(alist,0,len(alist)-1)

def quickSortHelper(alist,first,last):
    if last-first == 1:
        if alist[1] < alist[0]:
            temp = alist[0]
            alist[0] = alist[1]
            alist[1] = temp
##    elif first-last < 2:
##        alist = alist
##        print('hi')
    elif first<last:
       splitpoint1, splitpoint2 = partition(alist,first,last)
        # recursively calls quickSortHelper on the three sections of the list
##       print('front', alist[first:splitpoint1])
##       print('middle', alist[splitpoint1:splitpoint2])
##       print('end', alist[splitpoint2:last])
       
       quickSortHelper(alist,first,splitpoint1-1)
       #print(alist)
       quickSortHelper(alist,splitpoint1,splitpoint2-1)
       #print(alist)
       quickSortHelper(alist,splitpoint2,last)
       #print(alist)


def partition(alist,first,last): # uses the partitionOnce function to partition two times
    splitpoint1 = partitionOnce(alist,first,last)    

    # partitions the second half of the list using the first partition point as the first value
    splitpoint2 = partitionOnce(alist,splitpoint1+1,last)
   
    return splitpoint1, splitpoint2


def partitionOnce(alist,first,last): ## paritions a list once
   pivotvalue = alist[first]

   leftmark = first+1
   rightmark = last

   done = False
   while not done:

       while leftmark <= rightmark and alist[leftmark] <= pivotvalue:
           leftmark = leftmark + 1

       while alist[rightmark] >= pivotvalue and rightmark >= leftmark:
           rightmark = rightmark -1

       if rightmark < leftmark:
           done = True
       else:
           temp = alist[leftmark]
           alist[leftmark] = alist[rightmark]
           alist[rightmark] = temp

   temp = alist[first]
   alist[first] = alist[rightmark]
   alist[rightmark] = temp


   return rightmark




def main():
    lis1 = [90, 1, 3, 5, 6, 21, 43, 6, 76, 22, 111, 2 , 3 , 222]
    lis2 = [33,6,43,1,76,3,67,5,98]
    lis3 = [54,26,93]
    lis3 = [3,6,43,1,76,10,67,5,0, 100, 12, 5]
    print(lis3)
    mergeSort(lis1)
    #quickSort(lis3)

    print(lis1)
    #print(lis1)
    #print(partition(lis1,0,len(lis1)-1))

main()
