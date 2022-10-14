Report for Task 5:

#### Approach Overview
The data is presented in triplet format and the similarity associations within the triplets are known. A siamese network with triplet loss is therefore a suitable architecture for the task and is the focus of my work.
Firstly, I use a pretrained model (without training) to obtain image embeddings. I then build my own embedding model to be trained. Finally, I build the siamese network with the following steps:
         -Input triplet pretrained embeddings
         -Pass through trainable model
         -Concatenate output embeddings
         -Calculate triplet loss

#### Pretrained Model
I explored three common architectures trained on ImageNet: VGG16, InceptionV3 and ResNet50. To obtain the embeddings, I pass preprocessed images through the network and use the results from the second to last layer. The original images are resized to maintain a common aspect ratio and to reduce the resolution for speed. They are also scaled as required by each pretrained model. Of the three architectures I experimented with, ResNet50 yielded the best results on similar input image resolution. I have also experimented with a number of rescaled image resolutions. I have settled on an image resolution of 209x300 which strikes the balance between model performance and computational complexity.

#### Trainable Model
This model takes the pre-computed ResNet50 embeddings as input (size 7x10x2048). I tried many combinations of convolutional, max-pooling and dense layers. Most attempts resulted in significantly overfitting the training set. Finally, I settled on a relatively simple max-pooling layer followed by a dense output layer (size 64). Therefore, I found that when heavily regularised, this simpler architecture trained over a few epochs performed best.
In addition, RMSprop with a constant learning rate of 0.001 performed better than using the Adam optimizer. Also, the triplet loss margin hyperparameter also has a noticeable impact on performance.

#### Model Evaluation
To avoid data leakage and to obtain a reliable performance indication, I split the training triplets into train and test sets. This is done such that images in the test set do not appear at all in the train set triplets. The downside of this approach is that the train set is significantly reduced while the test set remains small.
