import numpy as np
from scipy.optimize import minimize
from math import sqrt
import pickle

'''
You need to modify the functions except for initializeWeights() 
'''


def initializeWeights(n_in, n_out):
    '''
    initializeWeights return the random weights for Neural Network given the
    number of node in the input layer and output layer

    Input:
    n_in: number of nodes of the input layer
    n_out: number of nodes of the output layer

    Output:
    W: matrix of random initial weights with size (n_out x (n_in + 1))
    '''
    epsilon = sqrt(6) / sqrt(n_in + n_out + 1)
    W = (np.random.rand(n_out, n_in + 1) * 2 * epsilon) - epsilon
    return W


def sigmoid(z):
    '''
    Notice that z can be a scalar, a vector or a matrix
    return the sigmoid of input z (same dimensions as z)
    '''
    # remove the next line and replace it with your code
    return 1 / (1 + np.exp(-z))


def nnObjFunction(params, *args):
    '''
    % nnObjFunction computes the value of objective function (cross-entropy
    % with regularization) given the weights and the training data and lambda
    % - regularization hyper-parameter.

    % Input:
    % params: vector of weights of 2 matrices W1 (weights of connections from
    %     input layer to hidden layer) and W2 (weights of connections from
    %     hidden layer to output layer) where all of the weights are contained
    %     in a single vector.
    % n_input: number of nodes in input layer (not including the bias node)
    % n_hidden: number of nodes in hidden layer (not including the bias node)
    % n_class: number of nodes in output layer (number of classes in
    %     classification problem
    % train_data: matrix of training data. Each row of this matrix
    %     represents the feature vector of the corresponding instance
    % train_label: the vector of true labels of training instances. Each entry
    %     in the vector represents the truee label of its corresponding training instance.
    % lambda: regularization hyper-parameter. This value is used for fixing the
    %     overfitting problem.

    % Output:
    % obj_val: a scalar value representing value of error function
    % obj_grad: a SINGLE vector (not a matrix) of gradient value of error function
    % NOTE: how to compute obj_grad
    % Use backpropagation algorithm to compute the gradient of error function
    % for each weights in weight matrices.
    '''
    # do not remove the next 5 lines
    n_input, n_hidden, n_class, train_data, train_label, lambdaval = args
    # First reshape 'params' vector into 2 matrices of weights W1 and W2

    W1 = params[0:n_hidden * (n_input + 1)].reshape((n_hidden, (n_input + 1)))
    W2 = params[(n_hidden * (n_input + 1)):].reshape((n_class, (n_hidden + 1)))

    # remove the next two lines and replace them with your code
    a = train_data.shape[0]
    train_label = train_label.astype(int)
    y = np.zeros((a, n_class))
    y[np.arange(train_label.size), train_label] = 1

    train_data = np.concatenate((train_data, np.ones((train_data.shape[0], 1))), axis=1)

    ## Feed Forward ##
    z = np.dot(train_data, W1.T)
    z1 = sigmoid(z)

    # Bias added
    z2 = np.concatenate((z1, np.ones((z1.shape[0], 1))), axis=1)

    # output
    o = sigmoid(np.dot(z2, W2.T))

    ## Backpropagation ##
    d = o - y
    w2_e = np.dot(d.T, z2)
    zh = np.multiply((1 - z2), z2)
    sigmaFunction = np.dot(d, W2)
    cal = np.multiply(zh, sigmaFunction)
    result = np.dot(cal.T, train_data)

    w1_g = (np.delete(result, n_hidden, 0) + lambdaval * W1) / a
    w2_g = (w2_e + lambdaval * W2) / a
    obj_grad = np.concatenate((w1_g.flatten(), w2_g.flatten()), axis=0)

    ## Calculate Error
    err = np.log(o) * y + np.log(1 - o) * (1 - y)
    err2 = np.sum(err)
    err_final = (-1 / a) * err2

    ## Calculate Regularization
    regular = lambdaval * ((np.sum(W1 ** 2) + np.sum(W2 ** 2)) / (2 * a))
    obj_val = err_final + regular

    return (obj_val, obj_grad)


def nnPredict(W1, W2, data):
    '''
    % nnPredict predicts the label of data given the parameter W1, W2 of Neural
    % Network.

    % Input:
    % W1: matrix of weights for hidden layer units
    % W2: matrix of weights for output layer units
    % data: matrix of data. Each row of this matrix represents the feature vector for the corresponding data instance

    % Output:
    % label: a column vector of predicted labels
    '''
    # remove the next line and replace it with your code
    data = np.concatenate((data, np.ones((data.shape[0], 1))), axis=1)
    z = sigmoid(np.dot(data, np.transpose(W1)))
    z = np.concatenate((z, np.ones((z.shape[0], 1))), axis=1)
    return np.argmax(sigmoid(np.dot(z, np.transpose(W2))), axis=1)
