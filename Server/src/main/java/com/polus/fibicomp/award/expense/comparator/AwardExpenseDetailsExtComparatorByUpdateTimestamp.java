package com.polus.fibicomp.award.expense.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetailsExt;

public class AwardExpenseDetailsExtComparatorByUpdateTimestamp implements Comparator<AwardExpenseDetailsExt> {

	@Override
	public int compare(AwardExpenseDetailsExt o1, AwardExpenseDetailsExt o2) {
		return new CompareToBuilder().append(o2.getUpdateTimeStamp(), o1.getUpdateTimeStamp()).toComparison();
	}

}
