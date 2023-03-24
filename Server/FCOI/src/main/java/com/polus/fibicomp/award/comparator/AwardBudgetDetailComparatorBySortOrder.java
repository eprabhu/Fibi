package com.polus.fibicomp.award.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;

public class AwardBudgetDetailComparatorBySortOrder implements Comparator<AwardBudgetDetail>{
	@Override
	public int compare(AwardBudgetDetail bd1, AwardBudgetDetail bd2) {
		return new CompareToBuilder().append(bd1.getBudgetCategory().getSortOrder(), bd2.getBudgetCategory().getSortOrder()).toComparison();
	}
}
