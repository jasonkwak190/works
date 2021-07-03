from sklearn.naive_bayes import MultinomialNB
from Preprocessing import preprocess
from utils import *
from Postprocessing import enforce_equal_opportunity
import copy


def naive_bayes_classification(metrics):
    training_data, training_labels, test_data, test_labels, categories, mappings = preprocess(metrics)

    NBC = MultinomialNB()
    NBC.fit(training_data, training_labels)

    data = np.concatenate((training_data, test_data))
    labels = np.concatenate((training_labels, test_labels))

    class_predictions = NBC.predict_proba(data)
    predictions = []

    for i in range(len(labels)):
        predictions.append(class_predictions[i][1])

    return data, predictions, labels, categories, mappings


metrics = ["race", "sex", "age", 'c_charge_degree', 'priors_count', 'c_charge_desc']
data, predictions, labels, categories, mappings = naive_bayes_classification(metrics)
race_cases = get_cases_by_metric(data, categories, "race", mappings, predictions, labels)
data = race_cases
#report_results(race_cases)
print("Attempting to enforce equal opportunity...")
equal_opportunity_data, equal_opportunity_thresholds = enforce_equal_opportunity(copy.deepcopy(data), 0.01)
if equal_opportunity_data is not None:
    print("--------------------EQUAL OPPORTUNITY RESULTS--------------------")
    print("")
    for group in equal_opportunity_data.keys():
        accuracy = get_num_correct(equal_opportunity_data[group]) / len(equal_opportunity_data[group])
        print("Accuracy for " + group + ": " + str(accuracy))

    print("")
    for group in equal_opportunity_data.keys():
        FPR = get_false_positive_rate(equal_opportunity_data[group])
        print("FPR for " + group + ": " + str(FPR))

    print("")
    for group in equal_opportunity_data.keys():
        FNR = get_false_negative_rate(equal_opportunity_data[group])
        print("FNR for " + group + ": " + str(FNR))

    print("")
    for group in equal_opportunity_data.keys():
        TPR = get_true_positive_rate(equal_opportunity_data[group])
        print("TPR for " + group + ": " + str(TPR))

    print("")
    for group in equal_opportunity_data.keys():
        TNR = get_true_negative_rate(equal_opportunity_data[group])
        print("TNR for " + group + ": " + str(TNR))

    print("")
    for group in equal_opportunity_thresholds.keys():
        print("Threshold for " + group + ": " + str(equal_opportunity_thresholds[group]))

    print("")
    total_cost = apply_financials(equal_opportunity_data)
    print("Total cost: ")
    print('${:,.0f}'.format(total_cost))
    total_accuracy = get_total_accuracy(equal_opportunity_data)
    print("Total accuracy: " + str(total_accuracy))
    print("-----------------------------------------------------------------")
    print("")

