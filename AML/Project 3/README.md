### Task 3:
Mitral valve segmentation of short ultrasound videos of a beating heart.

### Report for task 3:

For this task, I set aside three expert videos for validation. For training, I used the rest of the expert videos and all amateur videos.

I used the UNet architecture, mostly following the paper implementation. One change I made was to use the upsampling layers instead of transposed convolutions.

I then augmented the frames. I created a custom class in pytorch, where I defined all transformations used, split in several categories. During training, I applied a core set of transformations to both frames and labels, namely I centre-cropped the images, resized to (128, 128) and applied random rotations (0, +/-5, +/-10). Then, I used random brightness, contrast and added gaussian noise to the frames only. Equally, in the validation set, I used the same core transformations, this time without the rotations. I then created a DataLoader with a batch size of 10. 

I trained for 40 epochs using the Adam optimiser. For the loss, I used an equally weighted linear combination of the binary cross entropy with logits and the IoU score, as suggested in literature. For evaluation, I computed the mask applying a sigmoid with a 0.5 threshold to highlight the region of interest (ROI).

As a post-processing step, I used erosion to shrink the boundary of the ROI. I have also reverted the transformations applied to the masks in the training step to bring them back to their original size.

For the final result, I selected the model at epoch 30 as it performed best even on the more difficult videos.
