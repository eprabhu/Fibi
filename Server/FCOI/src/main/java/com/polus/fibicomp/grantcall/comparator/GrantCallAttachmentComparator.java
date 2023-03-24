package com.polus.fibicomp.grantcall.comparator;

import java.util.Comparator;
import org.apache.commons.lang3.builder.CompareToBuilder;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;

public class GrantCallAttachmentComparator implements Comparator<GrantCallAttachment> {

	@Override
	public int compare(GrantCallAttachment pa1, GrantCallAttachment pa2) {
		return new CompareToBuilder().append(pa1.getGrantAttachmentTypeCode(), pa2.getGrantAttachmentTypeCode())
				.append(pa1.getVersionNumber(), pa2.getVersionNumber()).toComparison();
	}
}
