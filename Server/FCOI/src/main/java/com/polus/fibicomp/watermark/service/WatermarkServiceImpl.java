package com.polus.fibicomp.watermark.service;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.sql.Date;

import javax.imageio.ImageIO;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.tomcat.util.http.fileupload.ByteArrayOutputStream;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.itextpdf.text.Element;
import com.itextpdf.text.Font;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.ColumnText;
import com.itextpdf.text.pdf.GrayColor;
import com.itextpdf.text.pdf.PdfContentByte;
import com.itextpdf.text.pdf.PdfGState;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.PdfStamper;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;

@Transactional
@Service(value = "watermarkService")
public class WatermarkServiceImpl implements WatermarkService {

	protected static Logger logger = LogManager.getLogger(WatermarkServiceImpl.class.getName());
	
	@Autowired
	private CommonService commonService;

	@Override
	public byte[] generateTimestampAndUsernameForPdf(byte[] data, Date updatedDate, String updateUser) {
		byte[] byteArray = null;
		try {
			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
			PdfReader reader = new PdfReader(data);
			PdfStamper watermark = new PdfStamper(reader, outputStream);
			Font FONT = new Font(Font.FontFamily.COURIER, 10, Font.BOLD, new GrayColor(0.5f));
			Phrase watermarkText = new Phrase("Uploaded on " + commonService.convertDateFormatBasedOnTimeZone(updatedDate.getTime(),Constants.DEFAULT_DATE_FORMAT) + " by " + updateUser, FONT);
			PdfContentByte over;
			Rectangle pageSize;
			float x;
			int totalpages = reader.getNumberOfPages();
			for (int i = 1; i <= totalpages; i++) {
				pageSize = reader.getPageSizeWithRotation(i);
				x = (pageSize.getLeft() + pageSize.getRight());
				over = watermark.getOverContent(i);
				over.saveState();
				PdfGState state = new PdfGState();
				state.setFillOpacity(10);
				over.setGState(state);
				ColumnText.showTextAligned(over, Element.ALIGN_CENTER, watermarkText, x - 150, 15, 0);
				over.restoreState();
			}
			watermark.close();
			reader.close();
			byteArray = outputStream.toByteArray();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return byteArray;
	}

	@Override
	public byte[] generateTimestampAndUsernameForImages(byte[] data, Date updatedDate, String updateUser, String contentType) {
		byte[] byteArray = null;
		try {
			ByteArrayInputStream inputStream = new ByteArrayInputStream(data);
			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
			BufferedImage sourceImage = ImageIO.read(inputStream);
			Graphics2D g2d = (Graphics2D) sourceImage.getGraphics();
			AlphaComposite alphaChannel = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f);
			g2d.setComposite(alphaChannel);
			g2d.setColor(Color.DARK_GRAY);
			String watermarkText = " Uploaded on " + commonService.convertDateFormatBasedOnTimeZone(updatedDate.getTime(),Constants.DEFAULT_DATE_FORMAT) + " by " + updateUser;
			int countOfChar = watermarkText.length();
			float centerX = sourceImage.getWidth();
			float centerY = sourceImage.getHeight() - 10;
			int size = ((sourceImage.getWidth() + sourceImage.getHeight()) / 2) / countOfChar;
			g2d.setFont(new java.awt.Font("Courier", Font.BOLD, size));
			FontMetrics fontMetrics = g2d.getFontMetrics();
			g2d.drawString(watermarkText, centerX - fontMetrics.stringWidth(watermarkText), centerY);
			String[] parts = contentType.split("/");
			String format = parts[1];
			ImageIO.write(sourceImage, format, outputStream);
			g2d.dispose();
			byteArray = outputStream.toByteArray();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return byteArray;
	}

}
