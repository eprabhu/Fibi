package com.polus.fibicomp.proposal.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;

public class AwardBudgetDetailComparatorByBudgetCategoryCode implements Comparator<AwardBudgetDetail> {

	@Override
	public int compare(AwardBudgetDetail bd1, AwardBudgetDetail bd2) {
		return new CompareToBuilder().append(bd1.getBudgetCategoryCode(), bd2.getBudgetCategoryCode()).toComparison();
	}

}
