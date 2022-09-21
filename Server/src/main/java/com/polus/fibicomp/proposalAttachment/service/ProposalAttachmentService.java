package com.polus.fibicomp.proposalAttachment.service;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Service
public interface ProposalAttachmentService {

	public String uploadMedia(String file, String name, Integer remaining, Integer length, Integer moduleCode, Long moduleItemKey, Long userId, String contentType);

	public String uploadMedia(MultipartFile file, String fileName);
}
