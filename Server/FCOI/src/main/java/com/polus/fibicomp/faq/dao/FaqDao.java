package com.polus.fibicomp.faq.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.faq.pojo.FaqAttachment;
import com.polus.fibicomp.faq.pojo.FaqCategory;
import com.polus.fibicomp.pojo.Faq;

@Service
public interface FaqDao {

	public List<FaqCategory> listFaqCategory();

	public Faq saveOrUpdateFaq(Faq faq);

	public List<Faq> fetchFaqByParams(Integer categoryCode, Integer subCategoryCode);

	public FaqAttachment fetchFaqAttachmentById(Integer faqAttachmentId);

	public List<FaqAttachment> fetchFaqAttachmentByfaqAttachmentId(Integer faqAttachmentId);

	public FaqAttachment deleteFaqAttachment(FaqAttachment faqAttachment);

	/**
	 * This method is used to fetchFaqById
	 * @param questionId - Id of a FAQ.
	 * @return An object of FAQ.
	 */
	public Faq fetchFaqById(Integer questionId);

}
