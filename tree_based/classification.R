confusion = data.frame('Test N' = c(150, 10000),
                       'Test P' = c(100, 50))
rownames(confusion) = c('No disease', 'Disease')

TN = confusion[1,1]
FP = confusion[1,2]
FN = confusion[2,1]
TP = confusion[2,2]
total = sum(confusion)
Disease = sum(confusion[2,]) #actualPositive
Nodisease = sum(confusion[1,])
TestedPositive = sum(confusion[,2])
TestedNegative = sum(confusion[,1])

# Accuracy: correct test
(accuracy = (TP+TN)/total)
# 1 - accuracy
(misclassificationRate = (FP+FN)/total)

# Sensitivity or Recall or 1 - Type II error, power, (TP/ (TP + FN))
# When FN is worse than FP (medical diagonois), we want a high recall
(recall = TP/Disease)
# 1 - specificity or Type I error
(falsePositiveRate = FP/Nodisease)
# Specificity or TN / (TN + FP)
(specificity = TN/Nodisease)
# Precision: When test is positive, how often is it correct? TP / (TP + FP)
# When FP is worse than FN (spam), we want a high precision
(precision = ifelse(TP==0,0,TP/TestedPositive))
# Prevalence is more important in Bayes 
(prevalence = Disease/total)


# Accuracy paradox means the accuracy may increase when you don't make a guess.
# To solve this paradox, people use Cohen’s Kappa.
# Unlike Accuracy, Kappa estimate of degenerative classifier is quite adequate. It is the same as for random classifier.
random_accuracy = (TN + FP)/total*(TN + FN)/total + (FN + TP)/total * (TP + FP)/total
Kappa = (accuracy - random_accuracy) / (1 - random_accuracy)
# If Kappa range from -1 to 1, where 0 represents the quality that can be expected from random classifier, while 1 represents perfect predicting quality.
# Kappa below 0 are theoretically possible but unlikely in practive.
print(Kappa)
# F-measure
# harmonic mean, which is always lower than arithmetic mean (x + y) /2
# F1 score raises flag when one of them is small
F1 = 2 * precision * recall / (precision + recall)
print(F1)
# F-measure's limitation?
# F_beta score
F1_beta = function(beta, precision, recall){
   (1 + beta^2) * precision * recall / (beta * precision + recall)
} 

