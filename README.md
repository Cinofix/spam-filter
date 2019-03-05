# SpamFilter_SVM_Bayes
Spam Filter classifier using SVM and Naive Bayes

## Description (by the Professor)
Write a spam filter using discrimitative and generative classifiers. Use the Spambase dataset which already represents spam/ham messages through a bag-of-words representations through a dictionary of 48 highly discriminative words and 6 characters. The first 54 features correspond to word/symbols frequencies; ignore features 55-57; feature 58 is the class label (1 spam/0 ham).
* Perform SVM classification using linear, polynomial of degree 2, and RBF kernels over the TF/IDF representation. Can you transform the kernels to make use of angular information only (i.e. no length)? Are they still positive definite kernels?
* Classify the same data also through a Naive Bayes classifier for continuous inputs, modelling each feature with a Gaussian distribution