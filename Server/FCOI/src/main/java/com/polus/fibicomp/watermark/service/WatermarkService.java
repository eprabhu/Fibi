package com.polus.fibicomp.watermark.service;

import java.sql.Date;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service(value = "watermarkService")
public interface WatermarkService {

	/**
	 * This method is used to Generate WaterMark on proposal PDF document
	 * attachments.
	 * 
	 * @param data       - Byte array of Attachment.
	 * @param attachment - Object of ProposalAttachment class.
	 * @return byte array of attachment pdf.
	 */
	public byte[] generateTimestampAndUsernameForPdf(byte[] data, Date updatedDate, String updateUser);

	/**
	 * This method is used to Generate WaterMark on proposal Image attachments.
	 * 
	 * @param data       - Byte array of Attachment.
	 * @param attachment - Object of ProposalAttachment class.
	 * @return byte array of attachment Image.
	 */
	public byte[] generateTimestampAndUsernameForImages(byte[] data, Date updatedDate, String updateUser, String contentType);

}
