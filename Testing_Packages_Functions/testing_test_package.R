# http://www.r-text.org/


library(text)
Language_based_assessment_data_8
x <- Language_based_assessment_data_8[1:2, 1:2]
# Example 1
wordembeddings <- textEmbed(x, layers = 9:11, context_layers = 11, decontext_layers = 9)
bert-base-uncased
