package com.polus.fibicomp.coi.service;

import com.polus.fibicomp.faq.vo.FaqCategoryVo;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Service
public interface FAQService {

    String listFaqCategory(FaqCategoryVo vo);

    String fetchFaqTable(FaqCategoryVo vo);

    String addFaqAttachment(MultipartFile[] files, String formDataJson);

    String saveUpdateFaq(FaqCategoryVo vo);
}
