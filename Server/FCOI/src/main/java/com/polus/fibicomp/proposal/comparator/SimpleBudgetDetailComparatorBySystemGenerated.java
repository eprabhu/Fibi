package com.polus.fibicomp.proposal.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.budget.vo.SimpleBudgetVO;

public class SimpleBudgetDetailComparatorBySystemGenerated implements Comparator<SimpleBudgetVO> {

	@Override
	public int compare(SimpleBudgetVO bd1, SimpleBudgetVO bd2) {
		return new CompareToBuilder().append(bd1.getIsSystemGeneratedCostElement(), bd2.getIsSystemGeneratedCostElement()).toComparison();
	}

}
