package com.polus.fibicomp.proposal.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.vo.PeriodCost;

public class SimpleBudgetDetailComparatorByBudgetPeriod implements Comparator<PeriodCost> {

	@Override
	public int compare(PeriodCost o1, PeriodCost o2) {
		return new CompareToBuilder().append(o1.getBudgetPeriod(), o2.getBudgetPeriod()).toComparison();
	}

}
