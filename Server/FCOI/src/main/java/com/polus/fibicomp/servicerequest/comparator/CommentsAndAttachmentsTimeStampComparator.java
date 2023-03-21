package com.polus.fibicomp.servicerequest.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.servicerequest.dto.CommentAndAttachmentHistory;

public class CommentsAndAttachmentsTimeStampComparator implements Comparator<CommentAndAttachmentHistory> {

	@Override
	public int compare(CommentAndAttachmentHistory cah1, CommentAndAttachmentHistory cah2) {
		return new CompareToBuilder().append(cah1.getUpdateTimeStamp().getTime(), cah2.getUpdateTimeStamp().getTime()).toComparison();
	}

}

