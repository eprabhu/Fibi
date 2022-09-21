package com.polus.fibicomp.proposal.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.vo.SimpleBudgetVO;

public class SimpleBudgetDetailComparatorByBudgetCategoryCode implements Comparator<SimpleBudgetVO> {

	@Override
	public int compare(SimpleBudgetVO o1, SimpleBudgetVO o2) {
		return new CompareToBuilder().append(o1.getCategoryName(), o2.getCategoryName()).toComparison();
	}

}
