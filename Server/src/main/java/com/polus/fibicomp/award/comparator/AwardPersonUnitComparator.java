package com.polus.fibicomp.award.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.award.pojo.AwardPersonUnit;

public class AwardPersonUnitComparator implements Comparator<AwardPersonUnit>{

	@Override
	public int compare(AwardPersonUnit ap1, AwardPersonUnit ap2) {
		return new CompareToBuilder().append(ap2.getLeadUnitFlag(), ap1.getLeadUnitFlag()).toComparison();
	}

}
