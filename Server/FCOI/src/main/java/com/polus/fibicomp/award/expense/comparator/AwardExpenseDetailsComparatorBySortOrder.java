package com.polus.fibicomp.award.expense.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.award.expense.vo.AwardExpenseDetailVO;

public class AwardExpenseDetailsComparatorBySortOrder implements Comparator<AwardExpenseDetailVO> {

	@Override
	public int compare(AwardExpenseDetailVO o1, AwardExpenseDetailVO o2) {
		return new CompareToBuilder().append(o1.getSortOrder(), o2.getSortOrder()).toComparison();
	}

}
