package com.polus.fibicomp.coi.dao;

import com.polus.fibicomp.faq.pojo.FaqCategory;
import com.polus.fibicomp.pojo.Faq;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface FAQDao {

    List<FaqCategory> listFaqCategory();

    List<Faq> fetchFaqByParams(Integer categoryCode, Integer subCategoryCode);

    Faq saveOrUpdateFaq(Faq faq);
}
