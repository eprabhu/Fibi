package com.polus.fibicomp.print.dto;

public class BudgetNonPersonDetail {

    private String description;
    private String internalOrderCode;
    private String lineItemCost;

    public BudgetNonPersonDetail() {
    }

    public BudgetNonPersonDetail(String description, String internalOrderCode, String lineItemCost) {
        this.description = description;
        this.internalOrderCode = internalOrderCode;
        this.lineItemCost = lineItemCost;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getInternalOrderCode() {
        return internalOrderCode;
    }

    public void setInternalOrderCode(String internalOrderCode) {
        this.internalOrderCode = internalOrderCode;
    }

    public String getLineItemCost() {
        return lineItemCost;
    }

    public void setLineItemCost(String lineItemCost) {
        this.lineItemCost = lineItemCost;
    }
}
