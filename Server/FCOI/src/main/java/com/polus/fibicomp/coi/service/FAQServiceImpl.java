package com.polus.fibicomp.coi.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.coi.dao.FAQDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.faq.pojo.FaqAttachment;
import com.polus.fibicomp.faq.vo.FaqCategoryVo;
import com.polus.fibicomp.pojo.Faq;
import com.polus.fibicomp.pojo.FileData;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.transaction.Transactional;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

@Service
@Transactional
public class FAQServiceImpl implements FAQService {

    @Autowired
    private FAQDao faqDao;

    @Autowired
    CommonDao commonDao;

    @Override
    public String listFaqCategory(FaqCategoryVo vo) {
        return commonDao.convertObjectToJSON(faqDao.listFaqCategory());
    }

    @Override
    public String fetchFaqTable(FaqCategoryVo vo) {
        try {
            Integer categoryCode = vo.getCategoryCode();
            Integer subCategoryCode = vo.getSubCategoryCode();
            vo.setFaq(faqDao.fetchFaqByParams(categoryCode, subCategoryCode));
            vo.setFaqCategory(faqDao.listFaqCategory());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return commonDao.convertObjectToJSON(vo);
    }

    @Override
    public String addFaqAttachment(MultipartFile[] files, String formDataJson) {
        FaqCategoryVo faqCategoryVo = null;
        try {
            ObjectMapper mapper = new ObjectMapper();
            faqCategoryVo = mapper.readValue(formDataJson, FaqCategoryVo.class);
            Faq faq = faqCategoryVo.getFaqdtls();
            List<FaqAttachment> faqAttachment = faq.getFaqAttachment();
            List<FaqAttachment> newFaqAttachment = faqCategoryVo.getNewFaqAttachment();
            List<FaqAttachment> faqAttachments = new ArrayList<FaqAttachment>();
            Boolean isReplaced = false;
            if (files != null) {
                for (int i = 0; i < files.length; i++) {
                    for (FaqAttachment newAttachment : newFaqAttachment) {
                        String replaceFileName = newAttachment.getFileName();
                        boolean isRenameRequired = false;
                        int count = 1;
                        isRenameRequired = checkForDuplication(newAttachment.getFileName(), faqAttachment);
                        while (isRenameRequired) {
                            replaceFileName = newAttachment.getFileName();
                            replaceFileName = generateFileName(replaceFileName, count);
                            count = count + 1;
                            isRenameRequired = checkForDuplication(replaceFileName, faqAttachment);
                        }

                        if (newAttachment.getFaqAttachmentId() != null) {
                            for (FaqAttachment attachment : faqAttachment) {
                                if (attachment.getFaqAttachmentId() != null
                                        && attachment.getFaqAttachmentId().equals(newAttachment.getFaqAttachmentId())) {
                                    FaqAttachment faqAttachmentList = new FaqAttachment();
                                    isReplaced = true;
                                    File file = new File(files[i].getOriginalFilename());
                                    String fileName = file.getName();
                                    faqAttachmentList = addNewFaqAttachment(newAttachment, files[i], fileName,
                                            replaceFileName, faq);
                                    faqAttachmentList.setFaq(faq);
                                    faqAttachments.add(faqAttachmentList);
                                }
                            }
                        } else {
                            File file = new File(files[i].getOriginalFilename());
                            String fileName = file.getName();
                            if (newAttachment.getFileName().equals(fileName)) {
                                FaqAttachment faqAttachmnts = new FaqAttachment();
                                faqAttachmnts = addNewFaqAttachment(newAttachment, files[i], fileName, replaceFileName,
                                        faq);
                                faqAttachments.add(faqAttachmnts);

                            }
                        }
                    }
                }
                faq.getFaqAttachment().addAll(faqAttachments);
            }
            if (isReplaced = true) {
                faq = faqDao.saveOrUpdateFaq(faq);

            }

            faqCategoryVo.setFaqdtls(faq);
        } catch (Exception e) {
            e.printStackTrace();
            throw new ApplicationException("Failed", Constants.JAVA_ERROR);
        }

        return commonDao.convertObjectToJSON(faqCategoryVo);
    }

    private String generateFileName(String replaceFileName, int count) {
        String fileNameSplit = replaceFileName.split("\\.")[0];
        String extension = replaceFileName.split("\\.")[1];
        return fileNameSplit + "(" + count + ")" + "." + extension;
    }

    private boolean checkForDuplication(String fileName, List<FaqAttachment> faqAttachment) {
        for (FaqAttachment attachment : faqAttachment) {
            if (fileName.equals(attachment.getFileName())) {
                return true;
            }
        }
        return false;
    }

    public FaqAttachment addNewFaqAttachment(FaqAttachment newFaqAttachmentDtl, MultipartFile file, String fileName,
                                             String replacedFileName, Faq faq) {
        FaqAttachment faqAttachment = new FaqAttachment();
        try {
            if (newFaqAttachmentDtl.getFileName().equals(fileName)) {
                faqAttachment.setFileName(replacedFileName);
                faqAttachment.setMimeType(file.getContentType());
                faqAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
                faqAttachment.setUpdateUser(newFaqAttachmentDtl.getUpdateUser());
                faqAttachment.setFaq(faq);
                faqAttachment.setMimeType(file.getContentType());
                FileData fileData = new FileData();
                fileData.setAttachment(file.getBytes());
                fileData = commonDao.saveFileData(fileData);
                faqAttachment.setFileDataId(fileData.getFileDataId());
                fileData.setAttachment(file.getBytes());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return faqAttachment;
    }

    @Override
    public String saveUpdateFaq(FaqCategoryVo vo) {
        Faq faq = vo.getFaq().get(0);
        faq.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        faq = faqDao.saveOrUpdateFaq(faq);
        vo.setFaqdtls(faq);
        return commonDao.convertObjectToJSON(vo);

    }
}
