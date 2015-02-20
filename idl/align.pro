;Script to assist cross correlation between images
;Finds (0,0) coordinate
;Assumes array (image) is data cube
;CALL align,dx,dy,k,image
;dx, the difference in x between the two images
;dy, the difference in y between the two images
;k,the value of image1(0,0,0) the image you are trying to align to
;image2,the larger image you are trying to align

pro align,dx=dx,dy=dy,k=k,image=image

FOR i=0,dx DO BEGIN
    FOR j=0,dy DO BEGIN
        diff=k-image(i,j,0)
        IF diff EQ 0 THEN print, 'Centre is:',i,j
    ENDFOR
ENDFOR
print,'No alignment found.'
END