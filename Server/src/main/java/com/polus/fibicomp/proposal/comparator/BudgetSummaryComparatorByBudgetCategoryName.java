package com.polus.fibicomp.proposal.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.vo.BudgetPeriodSummary;

public class BudgetSummaryComparatorByBudgetCategoryName implements Comparator<BudgetPeriodSummary> {

	@Override
	public int compare(BudgetPeriodSummary bps1, BudgetPeriodSummary bps2) {
		return new CompareToBuilder().append(bps1.getBudgetCategory(), bps2.getBudgetCategory()).toComparison();
	}

}
