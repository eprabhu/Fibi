package com.polus.fibicomp.ip.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.ip.pojo.InstituteProposalAttachments;

public class InstituteProposalAttachmentComparator implements Comparator<InstituteProposalAttachments> {

	@Override
	public int compare(InstituteProposalAttachments ip1, InstituteProposalAttachments ip2) {
		return new CompareToBuilder().append(ip1.getAttachmentTypeCode(), ip2.getAttachmentTypeCode())
				.append(ip1.getVersionNumber(), ip2.getVersionNumber()).toComparison();
	}

}
