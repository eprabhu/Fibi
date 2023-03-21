package com.polus.fibicomp.award.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.award.pojo.AwardAmountInfo;

public class DatesAndAmountComparator implements Comparator<AwardAmountInfo>{

	@Override
	public int compare(AwardAmountInfo am1, AwardAmountInfo am2) {
		return new CompareToBuilder().append(am1.getAwardAmountInfoId(), am2.getAwardAmountInfoId()).toComparison();
	}

}
