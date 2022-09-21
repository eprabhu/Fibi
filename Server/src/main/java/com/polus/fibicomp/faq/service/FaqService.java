package com.polus.fibicomp.faq.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.faq.vo.FaqCategoryVo;

@Service
public interface FaqService {

	public String listFaqCategory(FaqCategoryVo vo);

	public String fetchFaqTable(FaqCategoryVo vo);

	public String addFaqAttachment(MultipartFile[] files, String formDataJson);

	public String saveUpdateFaq(FaqCategoryVo vo);

	public ResponseEntity<byte[]> downloadFaqAttachment(Integer faqAttachmentid);

	public String deleteFaqAttachment(FaqCategoryVo vo);

	/**
	 * This method is used to add FaqAttachment ForWaf
	 * @param vo - object of FAQ.
	 * @return A String of details of FAQ.
	 */
	public String addFaqAttachmentForWaf(FaqCategoryVo vo);

}
