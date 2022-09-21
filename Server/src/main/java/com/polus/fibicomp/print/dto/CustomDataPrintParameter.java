package com.polus.fibicomp.print.dto;

import java.util.List;

public class CustomDataPrintParameter {

    private String columnLabel;
    private String customElementName;
    private String answer;
    private List<String> answersList;

    public String getColumnLabel() {
        return columnLabel;
    }

    public void setColumnLabel(String columnLabel) {
        this.columnLabel = columnLabel;
    }

    public String getCustomElementName() {
        return customElementName;
    }

    public void setCustomElementName(String customElementName) {
        this.customElementName = customElementName;
    }

    public String getAnswer() {
        return answer;
    }

    public void setAnswer(String answer) {
        this.answer = answer;
    }

    public List<String> getAnswersList() {
        return answersList;
    }

    public void setAnswersList(List<String> answersList) {
        this.answersList = answersList;
    }
}
