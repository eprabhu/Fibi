package com.polus.fibicomp.proposalAttachment.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;

import javax.transaction.Transactional;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;

@Transactional
@Service(value = "proposalAttachmentService")
public class ProposalAttachmentServiceImpl implements ProposalAttachmentService {

//	@Autowired
//	private ProposalAttachmentDao proposalAttachmentDao;

	protected static Logger logger = LogManager.getLogger(ProposalAttachmentServiceImpl.class.getName());

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Override
	public String uploadMedia(String file, String name, Integer remaining, Integer length, Integer moduleCode, Long moduleItemKey, Long userId, String contentType) {
		Boolean isFileComplete = false;
		try {
			new File("uploads").mkdir();
			FileOutputStream fileOutputStream = new FileOutputStream("uploads\\"+moduleCode.toString() + '_' + moduleItemKey + '_' + userId+name, true);
			if (length != null && remaining != null && (length > 0 && remaining <= 0)) {
				isFileComplete = true;
				fileOutputStream.write(Base64.getDecoder().decode(file.getBytes()));
				fileOutputStream.close();				
				String fileName = name;
				String originalFileName = name;
				File attachment = new File("uploads\\"+moduleCode.toString() + '_' + moduleItemKey + '_' + userId+name);
				FileInputStream input = new FileInputStream(attachment);
				MultipartFile result = new MockMultipartFile(fileName, originalFileName, contentType, IOUtils.toByteArray(input));

				ProposalAttachment proposalAttachment = new ProposalAttachment();
				try {
					proposalAttachment.setAttachmentType(commonDao.getProposalAttachmentType(1));
					proposalAttachment.setAttachmentTypeCode(1);
					proposalAttachment.setDescription("test");
					proposalAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					proposalAttachment.setUpdateUser("quickstart");
					proposalAttachment.setFileName(fileName);
					proposalAttachment.setMimeType(result.getContentType());
					proposalAttachment.setVersionNumber(1);
					FileData fileData = new FileData();
					fileData.setAttachment(result.getBytes());
					fileData = commonDao.saveFileData(fileData);
					proposalAttachment.setFileDataId(fileData.getFileDataId());
					proposalAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
					proposalAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
					proposalAttachment.setDocumentId(1);
					proposalAttachment.setAttachment(result.getBytes());
					proposalAttachment.setNarrativeStatus(commonDao.getNarrativeStatusByCode("C"));
					proposalAttachment.setNarrativeStatusCode("C");
					proposalAttachment.setProposalId(257);
					proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
					cleanDirectory(new File("uploads"), attachment);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			if (!isFileComplete) {
				fileOutputStream.write(Base64.getDecoder().decode(file.getBytes()));
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON("success");
	}

	void cleanDirectory(File dir, File fi) {
		for (File file : dir.listFiles()) {
			if (!file.getName().equals(fi.getName())) {
				try {
					Files.deleteIfExists(Paths.get(file.getAbsolutePath()));
				} catch (IOException e) {
					logger.info("exception in delete the file :" + e);
					e.printStackTrace();
				}
			}

		}
	}

	@Override
	public String uploadMedia(MultipartFile file, String fileName) {
		ProposalAttachment proposalAttachment = new ProposalAttachment();
		try {
			proposalAttachment.setAttachmentType(commonDao.getProposalAttachmentType(1));
			proposalAttachment.setAttachmentTypeCode(1);
			proposalAttachment.setDescription("test");
			proposalAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposalAttachment.setUpdateUser("quickstart");
			proposalAttachment.setFileName(fileName);
			proposalAttachment.setMimeType(file.getContentType());
			proposalAttachment.setVersionNumber(1);
			FileData fileData = new FileData();
			fileData.setAttachment(file.getBytes());
			fileData = commonDao.saveFileData(fileData);
			proposalAttachment.setFileDataId(fileData.getFileDataId());
			proposalAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
			proposalAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
			proposalAttachment.setDocumentId(1);
			proposalAttachment.setAttachment(file.getBytes());
			proposalAttachment.setNarrativeStatus(commonDao.getNarrativeStatusByCode("C"));
			proposalAttachment.setNarrativeStatusCode("C");
			proposalAttachment.setProposalId(259);
			proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON("success");
	}

}
