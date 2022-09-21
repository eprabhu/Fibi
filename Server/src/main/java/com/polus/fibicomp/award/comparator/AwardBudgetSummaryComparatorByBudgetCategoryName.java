package com.polus.fibicomp.award.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.vo.AwardBudgetPeriodSummary;

public class AwardBudgetSummaryComparatorByBudgetCategoryName implements Comparator<AwardBudgetPeriodSummary> {
	@Override
	public int compare(AwardBudgetPeriodSummary bps1, AwardBudgetPeriodSummary bps2) {
		return new CompareToBuilder().append(bps1.getBudgetCategory(), bps2.getBudgetCategory()).toComparison();
	}
}
