package com.polus.fibicomp.manpower.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.manpower.pojo.AwardManpower;

public class ManpowerComparator implements Comparator<AwardManpower>{

	@Override
	public int compare(AwardManpower manpowerTypeCode1, AwardManpower manpowerTypeCode2) {
		return new CompareToBuilder().append(manpowerTypeCode1.getManpowerTypeCode(), manpowerTypeCode2.getManpowerTypeCode())
				.append(manpowerTypeCode1.getManpowerTypeCode(), manpowerTypeCode2.getManpowerTypeCode()).toComparison();
	}

}
