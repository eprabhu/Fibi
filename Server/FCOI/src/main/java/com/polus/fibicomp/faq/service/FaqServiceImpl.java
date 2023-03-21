package com.polus.fibicomp.faq.service;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.faq.dao.FaqDao;
import com.polus.fibicomp.faq.pojo.FaqAttachment;
import com.polus.fibicomp.faq.vo.FaqCategoryVo;
import com.polus.fibicomp.pojo.Faq;
import com.polus.fibicomp.pojo.FileData;

@Transactional
@Service(value = "faqService")
public class FaqServiceImpl implements FaqService {
	protected static Logger logger = LogManager.getLogger(FaqServiceImpl.class.getName());
	@Autowired
	private FaqDao faqDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private CommonService commonService;

	@Override
	public String saveUpdateFaq(FaqCategoryVo vo) {
		Faq faq = vo.getFaq().get(0);
		faq.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		faq = faqDao.saveOrUpdateFaq(faq);
		vo.setFaqdtls(faq);
		return commonDao.convertObjectToJSON(vo);

	}

	@Override
	public String fetchFaqTable(FaqCategoryVo vo) {
		Integer categoryCode = vo.getCategoryCode();
		Integer subCategoryCode = vo.getSubCategoryCode();
		vo.setFaq(faqDao.fetchFaqByParams(categoryCode, subCategoryCode));
		vo.setFaqCategory(faqDao.listFaqCategory());
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
			if (isReplaced = true) {
				faq = faqDao.saveOrUpdateFaq(faq);

			}

			faqCategoryVo.setFaqdtls(faq);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return committeeDao.convertObjectToJSON(faqCategoryVo);
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
	public ResponseEntity<byte[]> downloadFaqAttachment(Integer faqAttachmentId) {
		FaqAttachment attachment = faqDao.fetchFaqAttachmentById(faqAttachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			byte[] data = fileData.getAttachment();
			HttpHeaders headers = new HttpHeaders();
			if (attachment.getMimeType() != null && !attachment.getMimeType().isEmpty()) {
				headers.setContentType(MediaType.parseMediaType(attachment.getMimeType()));
			}
			String filename = attachment.getFileName();
			headers.setContentDispositionFormData(filename, filename);
			headers.setContentLength(data.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<byte[]>(data, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Exception in downloadFaqAttachment {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String deleteFaqAttachment(FaqCategoryVo vo) {
		try {
			List<FaqAttachment> faqAttachments = faqDao.fetchFaqAttachmentByfaqAttachmentId(vo.getFaqAttachmentId());
			if (faqAttachments != null && !faqAttachments.isEmpty()) {
				for (FaqAttachment faqAttachment : faqAttachments) {
					commonDao.deleteFileData(commonDao.getFileDataById(faqAttachment.getFileDataId()));
					faqDao.deleteFaqAttachment(faqAttachment);
				}
			}
			vo.setNewFaqAttachment(getFaqAttachmentByfaqAttachmentId(vo.getFaqAttachmentId()));
			vo.setStatus(true);
		} catch (Exception e) {
			vo.setStatus(false);
			e.printStackTrace();
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	private List<FaqAttachment> getFaqAttachmentByfaqAttachmentId(Integer faqAttachmentId) {
		List<FaqAttachment> faqAttachments = new ArrayList<FaqAttachment>();
		faqAttachments = faqDao.fetchFaqAttachmentByfaqAttachmentId(faqAttachmentId);
		return faqAttachments;
	}

	@Override
	public String listFaqCategory(FaqCategoryVo vo) {
		return commonDao.convertObjectToJSON(faqDao.listFaqCategory());

	}

	@Override
	public String addFaqAttachmentForWaf(FaqCategoryVo vo) {
		try {
			MultipartFile multipartFile = null;
			String contentType = null;
			Faq faq = new Faq();
			List<FaqAttachment> faqAttachment = new ArrayList<>();
			if (vo.getFaqdtls().getQuestionId() != null) {
				faq = faqDao.fetchFaqById(vo.getFaqdtls().getQuestionId());
				faqAttachment =  faq.getFaqAttachment();
			} else {
				faq = faqDao.saveOrUpdateFaq(vo.getFaqdtls());
			}
			String name = vo.getFileName();
			String splicedFile = vo.getFileContent();
			Integer remaining = vo.getRemaining();
			Integer length = vo.getLength();
			String userId = vo.getPersonId();
			String timestamp = vo.getFileTimestamp();
			if (splicedFile != null) {
				contentType = vo.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if (multipartFile != null && !multipartFile.isEmpty() && vo.getRemaining() == 0) {
				String replaceFileName = vo.getFileName();
				boolean isRenameRequired = false;
				int count = 1;
				isRenameRequired = checkForDuplication(vo.getFileName(), faqAttachment);
				while (isRenameRequired) {
					replaceFileName = vo.getFileName();
					replaceFileName = generateFileName(replaceFileName, count);
					count = count + 1;
					isRenameRequired = checkForDuplication(replaceFileName, faqAttachment);
				}
					File file = new File(multipartFile.getOriginalFilename());
					String fileName = file.getName();
					if (vo.getFileName().equals(fileName)) {
						FaqAttachment faqAttachmentObj = new FaqAttachment();
						faqAttachmentObj.setFileName(replaceFileName);
						faqAttachmentObj.setMimeType(multipartFile.getContentType());
						faqAttachmentObj.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						faqAttachmentObj.setUpdateUser(faq.getUpdateUser());
						faqAttachmentObj.setFaq(faq);
						FileData fileData = new FileData();
						fileData.setAttachment(multipartFile.getBytes());
						fileData = commonDao.saveFileData(fileData);
						faqAttachmentObj.setFileDataId(fileData.getFileDataId());
						faq.getFaqAttachment().add(faqAttachmentObj);	
						vo.setFaqdtls(faqDao.saveOrUpdateFaq(faq));
					}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return committeeDao.convertObjectToJSON(vo);
	}

}
