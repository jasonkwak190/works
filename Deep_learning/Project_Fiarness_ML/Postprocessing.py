from utils import *
#######################################################################################################################
# YOU MUST FILL OUT YOUR SECONDARY OPTIMIZATION METRIC (either accuracy or cost)!
# The metric chosen must be the same for all 5 methods.
#
# Chosen Secondary Optimization Metric: accuracy
#######################################################################################################################
""" Determines the thresholds such that each group has equal predictive positive rates within 
    a tolerance value epsilon. For the Naive Bayes Classifier and SVM you should be able to find
    a nontrivial solution with epsilon=0.02. 
    Chooses the best solution of those that satisfy this constraint based on chosen 
    secondary optimization criteria.
"""
def enforce_demographic_parity(categorical_results, epsilon):
    demographic_parity_data = {}
    thresholds = {}

    # Must complete this function!
    pp = {}  # Dictionary of list of Predicted Positives(for every threshold) for every group
    for group in categorical_results.keys():
        perGroupList = []
        t = 0.00
        while (t <= 1):
            threshed = apply_threshold(categorical_results[group], t)
            perGroupList.append(get_num_predicted_positives(threshed)/len(threshed))
            t = t + 0.01
        pp[group] = perGroupList

    threshItr = epsilon
    samePPList = []  # A list with every row having dictionaries of thresholds corresponding to same Predicted Positives
    while (threshItr < 1):
        sTDict = {}
        emptyFlag = 0
        for group in pp.keys():
            sTDict[group] = list(i / 100 for i in range(100) if threshItr - epsilon <= pp[group][i] <= threshItr + epsilon)
            if (len(sTDict[group]) == 0):
                emptyFlag = 1
                break
        threshItr = threshItr + 0.01
        if (emptyFlag == 0):
            temp = {}
            for group in sTDict:
                maxA = 0
                bestT = sTDict[group][0]
                for t in sTDict[group]:
                    acc=get_num_correct(apply_threshold(categorical_results[group], t))
                    if(acc > maxA):
                        maxA=acc
                        bestT=t
                temp[group] = bestT
            samePPList.append(temp)

    maxAccuracy = 0
    for thresh in samePPList:
        tempDict = {}
        for group in categorical_results.keys():
            threshed = apply_threshold(categorical_results[group], thresh[group])
            tempDict[group] = threshed
        acc = get_total_accuracy(tempDict)
        if (acc > maxAccuracy):
            maxAccuracy = acc
            demographic_parity_data = tempDict
            thresholds = thresh
    return demographic_parity_data, thresholds


#######################################################################################################################
""" Determine thresholds such that all groups have equal TPR within some tolerance value epsilon, 
    and chooses best solution according to chosen secondary optimization criteria. For the Naive 
    Bayes Classifier and SVM you should be able to find a non-trivial solution with epsilon=0.01
"""
def enforce_equal_opportunity(categorical_results, epsilon):

    thresholds = {}
    equal_opportunity_data = {}

    # Must complete this function!
    tpr = {}    # Dictionary of list of TPRs(for every threshold) for every group
    for group in categorical_results.keys():
        perGroupList = []
        t = 0.00
        while (t <= 1):
            threshed = apply_threshold(categorical_results[group], t)
            perGroupList.append(get_true_positive_rate(threshed))
            t = t + 0.01
        tpr[group] = perGroupList

    threshItr = epsilon
    sameTprList = []    # A list with every row having dictionaries of thresholds corresponding to same TPR
    while(threshItr<1):
        sTDict = {}
        emptyFlag = 0
        for group in tpr.keys():
            sTDict[group] = list(i/100 for i in range(100) if threshItr-epsilon <= tpr[group][i] <= threshItr+epsilon)
            if (len(sTDict[group]) == 0):
                emptyFlag = 1
                break
        threshItr = threshItr + 0.01
        if(emptyFlag == 0):
            temp = {}
            for group in sTDict:
                maxA = 0
                bestT = sTDict[group][0]
                for t in sTDict[group]:
                    acc = get_num_correct(apply_threshold(categorical_results[group], t))
                    if (acc > maxA):
                        maxA = acc
                        bestT = t
                temp[group] = bestT
            sameTprList.append(temp)

    maxAccuracy = 0
    for thresh in sameTprList:
        tempDict = {}
        for group in categorical_results.keys():
            threshed = apply_threshold(categorical_results[group], thresh[group])
            tempDict[group] = threshed
        acc = get_total_accuracy(tempDict)
        if (acc > maxAccuracy):
            maxAccuracy = acc
            equal_opportunity_data = tempDict
            thresholds = thresh

    return equal_opportunity_data, thresholds

#######################################################################################################################

"""Determines which thresholds to use to achieve the maximum profit or maximum accuracy with the given data
"""

def enforce_maximum_profit(categorical_results):
    mp_data = {}
    thresholds = {}

    # Must complete this function!
    for group in categorical_results.keys():
        t = 0.00
        maxAccuracy = 0
        l=[]
        while (t <= 1):
            threshed = apply_threshold(categorical_results[group], t)
            acc = get_num_correct(threshed) / len(threshed)
            if (acc > maxAccuracy):
                maxAccuracy = acc
                mp_data[group] = threshed
                thresholds[group] = t
            t = t + 0.01
    return mp_data, thresholds

#######################################################################################################################
""" Determine thresholds such that all groups have the same PPV, and return the best solution
    according to chosen secondary optimization criteria
"""

def enforce_predictive_parity(categorical_results, epsilon):
    predictive_parity_data = {}
    thresholds = {}

    # Must complete this function!
    ppv = {}  # Dictionary of list of PPVs(for every threshold) for every group
    for group in categorical_results.keys():
        perGroupList = []
        t = 0.00
        while (t <= 1):
            threshed = apply_threshold(categorical_results[group], t)
            perGroupList.append(get_positive_predictive_value(threshed))
            t = t + 0.01
        ppv[group] = perGroupList

    threshItr = epsilon
    samePpvList = []  # A list with every row having dictionaries of thresholds corresponding to same PPV
    while (threshItr < 1):
        sTDict = {}
        emptyFlag = 0
        for group in ppv.keys():
            sTDict[group] = list(i / 100 for i in range(100) if threshItr - epsilon <= ppv[group][i] <= threshItr + epsilon)
            if (len(sTDict[group]) == 0):
                emptyFlag = 1
                break
        threshItr = threshItr + 0.01
        if (emptyFlag == 0):
            temp = {}
            for group in sTDict:
                maxA = 0
                bestT = sTDict[group][0]
                for t in sTDict[group]:
                    acc = get_num_correct(apply_threshold(categorical_results[group], t))
                    if (acc > maxA):
                        maxA = acc
                        bestT = t
                temp[group] = bestT
            samePpvList.append(temp)

    maxAccuracy = 0
    for thresh in samePpvList:
        tempDict = {}
        for group in categorical_results.keys():
            threshed = apply_threshold(categorical_results[group], thresh[group])
            tempDict[group] = threshed
        acc = get_total_accuracy(tempDict)
        if (acc > maxAccuracy):
            maxAccuracy = acc
            predictive_parity_data = tempDict
            thresholds = thresh
    return predictive_parity_data, thresholds


    ###################################################################################################################
""" Apply a single threshold to all groups, and return the best solution according to 
    chosen secondary optimization criteria
"""

def enforce_single_threshold(categorical_results):
    single_threshold_data = {}
    thresholds = {}

    # Must complete this function!
    t = 0.00
    optimalT = t
    maxAccuracy=0

    while(t<=1):
        tempDict = {}
        for group in categorical_results.keys():
            threshed=apply_threshold(categorical_results[group],t)
            tempDict[group]=threshed
        acc=get_total_accuracy(tempDict)
        if(acc>maxAccuracy):
            maxAccuracy=acc
            single_threshold_data=tempDict
            optimalT=t
        t=t+0.01
    for group in categorical_results.keys():
        thresholds[group]=optimalT
    return single_threshold_data, thresholds