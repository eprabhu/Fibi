package com.polus.fibicomp.common.service;

import java.beans.PropertyDescriptor;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipOutputStream;

import javax.persistence.Column;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackagePart;
import org.apache.poi.openxml4j.opc.PackagePartName;
import org.apache.poi.openxml4j.opc.PackagingURIHelper;
import org.apache.poi.ss.usermodel.Footer;
import org.apache.poi.ss.usermodel.Header;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFPictureData;
import org.apache.poi.xssf.usermodel.XSSFRelation;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.itextpdf.text.pdf.PdfWriter;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.event.PdfHeaderFooterPageEvent;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.pojo.LookupWindow;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.pojo.ArgValueLookup;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.LetterTemplateType;
import com.polus.fibicomp.pojo.ResearchTypeArea;
import com.polus.fibicomp.pojo.ResearchTypeSubArea;
import com.polus.fibicomp.pojo.WebSocketConfiguration;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.report.vo.VmlDrawing;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.service.ServiceRequestService;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;
import com.polus.fibicomp.util.Truth;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.LookUp;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;

@Transactional
@Service(value = "commonService")
public class CommonServiceImpl implements CommonService {

    protected static Logger logger = LogManager.getLogger(CommonServiceImpl.class.getName());

    protected static final String FISCAL_YEAR_MONTH_PARAMETER_NAME = "FISCAL_START_MONTH";
    protected static final String KC_GENERAL_NAMESPACE = "KC-GEN";
    protected static final String DOCUMENT_COMPONENT_NAME = "Document";
    protected static final String MONTH_KEY = "month";
    protected static final String YEAR_KEY = "year";

    @Autowired
    private CommonDao commonDao;

    @Autowired
    private ProposalService proposalService;

    @Autowired
    private ServiceRequestService serviceRequestService;

	@Autowired
	private WorkflowDao workflowDao;

    @Value("${notification.attachment.filepath}")
    private String notificationAttachmentFilePath;
    
    @Autowired
    private SftpConfigurationService sftpConfigurationService;

    @Override
    public Long getNextSequenceNumber(String sequenceName) {
        return commonDao.getNextSequenceNumber(sequenceName);
    }

    @Override
    public boolean getParameterValueAsBoolean(String parameterName) {
        return commonDao.getParameterValueAsBoolean(parameterName);
    }

    @Override
    public Integer getCurrentFiscalYear() {
        return getCurrentFiscalData(null).get(YEAR_KEY);
    }

    @Override
    public Integer getCurrentFiscalMonthForDisplay() {
        return getCurrentFiscalData(null).get(MONTH_KEY) + 1;
    }

    protected Integer getFiscalYearMonth() {
        return commonDao.getParameter(FISCAL_YEAR_MONTH_PARAMETER_NAME);
    }

    private int findMonth(int startingMonth, int currentMonth) {
        /**
         * We are building an array of integers. The array position number is the fiscal
         * month position of the calendar month. The array values are the calendar
         * months. So an example with a fiscal year starting in September would be as
         * follows: YEAR[0] = Calendar.September Year[1] = Calendar.October Year[11 =
         * Calendar.August
         */
        int nextMonth = startingMonth;
        int[] YEAR = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        for (int i = 0; i < 12; i++) {
            YEAR[i] = nextMonth;
            if (nextMonth == 11) {
                nextMonth = 0;
            } else {
                nextMonth++;
            }
        }
        for (int i : YEAR) {
            if (YEAR[i] == currentMonth) {
                return i;
            }
        }
        throw new IllegalArgumentException("Could not find the current month: " + currentMonth);
    }

    protected Map<String, Integer> getCurrentFiscalData(Calendar calendar) {
        Map<String, Integer> data = new HashMap<String, Integer>();

        if (calendar == null) {
            calendar = Calendar.getInstance();
        }
        Integer fiscalStartMonth = getFiscalYearMonth();
        // assuming July is the fiscal start month
        if (calendar.get(Calendar.MONTH) == fiscalStartMonth.intValue()) {
            // July 1st, 2012, is the 1st month of FY 2013
            data.put(MONTH_KEY, findMonth(fiscalStartMonth, calendar.get(Calendar.MONTH)));
            if (fiscalStartMonth.equals(Calendar.JANUARY)) {
                data.put(YEAR_KEY, calendar.get(Calendar.YEAR));
            } else {
                data.put(YEAR_KEY, calendar.get(Calendar.YEAR) + 1);
            }
        } else if (calendar.get(Calendar.MONTH) > fiscalStartMonth.intValue()) {
            // August 1st 2012, is the second month of FY 2013
            data.put(MONTH_KEY, findMonth(fiscalStartMonth, calendar.get(Calendar.MONTH)));
            if (fiscalStartMonth.equals(Calendar.JANUARY)) {
                data.put(YEAR_KEY, calendar.get(Calendar.YEAR));
            } else {
                data.put(YEAR_KEY, calendar.get(Calendar.YEAR) + 1);
            }
        } else {
            // June 1st 2012, is the 12th month of FY 2012
            data.put(MONTH_KEY, findMonth(fiscalStartMonth, calendar.get(Calendar.MONTH)));
            data.put(YEAR_KEY, calendar.get(Calendar.YEAR));
        }
        return data;
    }

    @Override
    public String hash(Object valueToHide) throws GeneralSecurityException {
        if (valueToHide != null && !StringUtils.isEmpty(valueToHide.toString())) {
            try {
                MessageDigest md = MessageDigest.getInstance(Constants.HASH_ALGORITHM);
                return new String(Base64.encodeBase64(md.digest(valueToHide.toString().getBytes(Constants.CHARSET))),
                        Constants.CHARSET);
            } catch (UnsupportedEncodingException arg2) {
                return "";
            }
        } else {
            return "";
        }
    }

    @Override
    public String render(String text, Map<String, String> replacementParameters) {
        for (String key : replacementParameters.keySet()) {
            text = StringUtils.replace(text, key, replacementParameters.get(key));
        }
        return text;
    }

    @Override
    public void sendMailAtEvent(NotificationType notificationType, Integer moduleTypeId, String moduleCode) {
        /*
         * Map<String,String> replacementParameters = new HashMap<>(); Integer mCode =
         * Integer.parseInt(moduleCode);
         * if(Constants.DEV_PROPOSAL_MODULE_CODE.equals(mCode)) { Proposal proposal =
         * proposalDao.fetchProposalById(moduleTypeId); replacementParameters =
         * proposalNotificationRenderService.getDefaultReplacementParameters(proposal);
         * } else if(Constants.NEGOTIATION_MODULE_CODE.equals(mCode)) { Negotiations
         * negotiations = negotiationDao.fetchNegotiationById(moduleTypeId);
         * replacementParameters = negotiationNotificationRenderService.
         * getNegotiationDefaultReplacementParameters(negotiations); } String subject =
         * render(notificationType.getSubject(), replacementParameters); String message
         * = render(notificationType.getMessage(), replacementParameters); Set<String>
         * toAddresses = new HashSet<String>(); Set<String> ccAddresses = new
         * HashSet<String>(); Set<String> bccAddresses = new HashSet<String>();
         * List<PersonDTO> person = new ArrayList<PersonDTO>(); Set<String>
         * recipientPersonId = new HashSet<String>();
         * if(notificationType.getIsActive().equals("Y")) { for (NotificationRecipient
         * notificationRecipient : notificationType.getNotificationRecipient()) { if
         * (notificationRecipient.getRoleTypeCode() == null) { String emailId =
         * emailMaintenanceDao.getEmail(notificationRecipient.getRecipientPersonId());
         * recipientPersonId.add(notificationRecipient.getRecipientPersonId()); if
         * (notificationRecipient.getRecipientType().equals("TO")) {
         * toAddresses.add(emailId); } else if
         * (notificationRecipient.getRecipientType().equals("CC")) {
         * ccAddresses.add(emailId); } else { bccAddresses.add(emailId); } } else {
         * person =
         * emailMaintenanceDao.getRoleEmail(notificationRecipient.getRoleTypeCode(),
         * notificationType.getModuleCode(), moduleTypeId.toString()); for (PersonDTO
         * personDTO : person) { recipientPersonId.add(personDTO.getPersonID()); if
         * (notificationRecipient.getRecipientType().equals("TO")) {
         * toAddresses.add(personDTO.getEmail()); } else if
         * (notificationRecipient.getRecipientType().equals("CC")) {
         * ccAddresses.add(personDTO.getEmail()); } else {
         * bccAddresses.add(personDTO.getEmail()); }
         *
         * } } } fibiEmailService.sendEmail(recipientPersonId,toAddresses, subject,
         * ccAddresses, bccAddresses, message, true); }
         */
    }

    @Override
    public String fibiFaq() {
        String responce;
        responce = commonDao.convertObjectToJSON(commonDao.fibiFaq());
        return responce;
    }

    @Override
    public void sendMailForAssignReview(NotificationType notificationType, ProposalReview proposalReview,
                                        Map.Entry<String, Integer> grantCallEntry, String reviewerRole, Proposal proposal) {
        ProposalVO proposalVO = new ProposalVO();
        proposalVO.setProposal(proposal);
        Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
        setNotificationRecipients(proposalReview.getReviewerPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
        if (notificationType.getNotificationTypeId() == Constants.APPLICATION_REVIEW_NOTIFICATION_CODE) {
            proposalVO.setNumberOfApplications(grantCallEntry.getValue());
            if (proposalReview.getReviewDeadLineDate() != null) {
                proposalVO.setReviewDeadLineDate(new Timestamp(proposalReview.getReviewDeadLineDate().getTime()));
            }
            proposalVO.setReviewerRole(reviewerRole);
            proposalService.sendProposalNotification(proposalVO, Constants.APPLICATION_REVIEW_NOTIFICATION_CODE, dynamicEmailrecipients);
        } else if (notificationType.getNotificationTypeId() == Constants.APPLICATION_ENDORSEMENT_NOTIFICATION_CODE) {
            proposalVO.setNumberOfApplications(grantCallEntry.getValue());
            proposalVO.setReviewerRole(reviewerRole);
            proposalService.sendProposalNotification(proposalVO, Constants.APPLICATION_ENDORSEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
        } else if (notificationType.getNotificationTypeId() == Constants.IRB_ASSESSMENT_NOTIFICATION_CODE) {
            proposalVO.setNumberOfApplications(grantCallEntry.getValue());
            proposalService.sendProposalNotification(proposalVO, Constants.IRB_ASSESSMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
        }
    }

    @Override
    public void sendMailForNotifyAction(NotificationType notificationType, Set<String> emailIds, String userName,
                                        String comment, String message, ServiceRequest serviceRequest) {
        Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
        for (String emailId : emailIds) {
            setNotificationRecipients(emailId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
        }
        ServiceRequestVO serviceRequestVO = new ServiceRequestVO();
        serviceRequestVO.setNotifyActionComment(comment);
        serviceRequestVO.setPersonName(userName);
        serviceRequestVO.setMessage(message);
        serviceRequestVO.setServiceRequest(serviceRequest);
        serviceRequestService.sendServiceRequestNotification(serviceRequestVO, notificationType.getNotificationTypeId(), dynamicEmailrecipients, null);
    }

    @Override
    public List<ArgValueLookup> getArgValueLookupData(String argumentName) {
        return commonDao.getArgValueLookupData(argumentName);
    }

    @Override
    public String getNotificationPlaceholder(Integer moduleCode) {
        List<ArgValueLookup> argumentList = getArgValueLookupData(getPlaceholderForModule(moduleCode));
        return commonDao.convertObjectToJSON(argumentList);
    }

    private String getPlaceholderForModule(Integer moduleCode) {
        switch (moduleCode) {
            case 1:
                return Constants.PLACEHOLDER_AWARD;
            case 2:
                return Constants.PLACEHOLDER_INSTITUTE_PROPOSAL;
            case 3:
                return Constants.PLACEHOLDER_DEV_PROPOSAL;
            case 5:
                return Constants.PLACEHOLDER_NEGOTIATION;
            case 15:
                return Constants.PLACEHOLDER_GRANT_CALL;
            case 20:
                return Constants.PLACEHOLDER_SERVICE_REQUEST;
            case 14:
                return Constants.PLACEHOLDER_CLAIM;
            case 21:
            	return Constants.PLACEHOLDER_EXTERNAL_USER;
            case 16:
            	return Constants.PLACEHOLDER_PROGRESS_REPORT;
            case 13:
			       return Constants.PLACEHOLDER_AGREEMENT;
        }
        return "";
    }

    @Override
    public String replaceHtmlFromEditor(String htmlContent) {
        return htmlContent.toString().replaceAll("\\<.*?>", "");
    }

    @Override
    public void setNotificationRecipients(String recipient, String recipientType, Set<NotificationRecipient> dynamicEmailrecipients) {
        NotificationRecipient notificationRecipient = new NotificationRecipient();
        notificationRecipient.setRecipientPersonId(recipient);
        notificationRecipient.setRecipientType(recipientType);
        dynamicEmailrecipients.add(notificationRecipient);
    }

    @SuppressWarnings("resource")
    @Override
    public MultipartFile uploadMedia(String file, String name, Integer remaining, Integer length, String timestamp,
                                     String userId, String contentType) {
        try {
            new File("uploads").mkdir();
            FileOutputStream fileOutputStream = new FileOutputStream("uploads\\" + userId + '_' + timestamp + '_' + name.toString(), true);
            if (length != null && remaining != null && (length > 0 && remaining <= 0)) {
                fileOutputStream.write(java.util.Base64.getDecoder().decode(file.getBytes()));
                fileOutputStream.close();
                String fileName = name;
                String originalFileName = name;
                File attachment = new File("uploads\\" + userId + '_' + timestamp + '_' + name.toString());
                FileInputStream input = new FileInputStream(attachment);
                MultipartFile result = new MockMultipartFile(fileName, originalFileName, contentType, IOUtils.toByteArray(input));
                input.close();
                cleanDirectory(new File("uploads"), attachment);
                return result;
            }
            fileOutputStream.write(java.util.Base64.getDecoder().decode(file.getBytes()));
            fileOutputStream.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    void cleanDirectory(File dir, File fi) {
        for (File file : dir.listFiles()) {
            if (!file.getName().equals(fi.getName())) {
                try {
                    Files.deleteIfExists(Paths.get(file.getAbsolutePath()));
                } catch (IOException e) {
                    return;
                }
            }

        }
    }

    @Override
    public String getLookUpDatas(String lookUpTableName, String lookUpTableColumnName) {
        List<LookUp> lookUpDatas = commonDao.getLookUpDatas(lookUpTableName, lookUpTableColumnName);
        return commonDao.convertObjectToJSON(lookUpDatas);
    }

    @Override
    public String convertDateFormatBasedOnTimeZone(Long dateValue, String dateFormat) {
        Date date = new Date(dateValue);
        String formattedDate = new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
        return formattedDate;
    }

    @Override
    public void setNotificationRecipientsforNonEmployees(String emailAddress, String recipientType, Set<NotificationRecipient> dynamicEmailrecipients) {
        NotificationRecipient notificationRecipient = new NotificationRecipient();
        notificationRecipient.setEmailAddress(emailAddress);
        notificationRecipient.setRecipientType(recipientType);
        dynamicEmailrecipients.add(notificationRecipient);
    }

    @Override
    public File createfileInUploads(XSSFWorkbook workbook, String fileName) {
        File report = null;
        try {
            File directory = new File(notificationAttachmentFilePath + File.separator + "NotificationAttatchments");
            directory.mkdir();
            String fileNameWithPath = File.separator + fileName;
            report = new File(directory, fileNameWithPath);
            FileOutputStream outputStream = new FileOutputStream(report);
            workbook.write(outputStream);
            workbook.close();
            outputStream.close();
        } catch (Exception e) {
            logger.error("error in createfileInUploads {}", e.getMessage());
        }
        return report;
    }

    @Override
    public Object getValueBasedOnColumnName(Object entityObject, String referanceColumn) {
        Object value = null;
        try {
            Class<? extends Object> className = entityObject.getClass();
            for (Field field : className.getDeclaredFields()) {
                Column col = field.getAnnotation(Column.class);
                if (col != null && referanceColumn.equals(col.name())) {
                    value = invokeGetter(entityObject, field.getName());
                }
            }
        } catch (Exception e) {
            logger.error("Error occuerd in invokeGetter : {}", e.getMessage());
        }
        return value;
    }

    public Object invokeGetter(Object entityObject, Object fieldName) {
        PropertyDescriptor pd;
        try {
            pd = new PropertyDescriptor((String) fieldName, entityObject.getClass());
            return pd.getReadMethod().invoke(entityObject);
        } catch (Exception e) {
            logger.error("Error occuerd in invokeGetter : {}", e.getMessage());
            return null;
        }
    }

    @Override
    public Integer addFilesToZipFolder(Integer index, String fileName, ZipOutputStream zos) throws IOException {
        try {
            zos.putNextEntry(new ZipEntry(fileName));
        } catch (ZipException e) {
            if (e.getMessage().contains("duplicate entry")) {
                zos.putNextEntry(new ZipEntry(chageDupliacteName(fileName, ++index)));
            }
        }
        return index;
    }

    private String chageDupliacteName(String fileName, Integer index) {
        int extIndex = fileName.lastIndexOf('.');
        return new StringBuilder().append(fileName.substring(0, extIndex)).append("(").append(index).append(")").append(fileName.substring(extIndex)).toString();
    }

    @Override
    public Claims getLoginPersonDetailFromJWT(HttpServletRequest request) {
    	String cookieToken = getTokenFromCookie(request);
    	if(cookieToken!=null) {
    		return Jwts.parser().setSigningKey(Constants.SECRET).parseClaimsJws(cookieToken).getBody();
    	}
        return Jwts.parser().setSigningKey(Constants.SECRET).parseClaimsJws(getJwtFromRequest(request)).getBody();
    }

    private String getTokenFromCookie(HttpServletRequest request) {
    	Cookie[] cookies = request.getCookies();
		if (cookies != null) {
			for (Cookie cookie : cookies) {
				if (cookie.getName().equals("Cookie_Token")) {
					return cookie.getValue();
				}
			}
		}
		return null;
	}

	public String getJwtFromRequest(HttpServletRequest request) {
		String cookieToken = getTokenFromCookie(request);
    	if(cookieToken!=null) {
    		return cookieToken;
    	}
        String bearerToken = request.getHeader(Constants.HEADER_STRING);
        if (org.springframework.util.StringUtils.hasText(bearerToken) && bearerToken.startsWith(Constants.TOKEN_PREFIX)) {
            return bearerToken.substring(7, bearerToken.length());
        }
        return null;
    }

    @Override
    public List<ResearchTypeArea> findResearchTypeArea(String searchString, String researchTypeCode) {
        return commonDao.findResearchTypeArea(searchString, researchTypeCode);
    }

    @Override
    public List<ResearchTypeSubArea> findResearchTypeSubArea(String searchString, String researchTypeAreaCode, String researchTypeCode) {
        return commonDao.findResearchTypeSubArea(searchString, researchTypeAreaCode, researchTypeCode);
    }

    @Override
    public void addDetailsInHeader(XSSFWorkbook workbook, XSSFSheet sheet) {
        try {
            Header header;
            InputStream is;
            byte[] bytes;
            int pictureIdx;
            header = sheet.getHeader();
            header.setLeft("&G");
            header.setCenter("&K000000&12" + Constants.REPORT_HEADER);
            Footer footer;
            footer = sheet.getFooter();
            footer.setCenter(Constants.REPORT_FOOTER);
            Resource resource = new ClassPathResource("logo.png");
            is = resource.getInputStream();
            bytes = IOUtils.toByteArray(is);
            pictureIdx = workbook.addPicture(bytes, Workbook.PICTURE_TYPE_PNG);
            is.close();
            sheet.setMargin(org.apache.poi.ss.usermodel.Sheet.TopMargin, 1);
            //create header picture from picture data of this workbook
            createPictureForHeader(sheet, pictureIdx, "logo", 1, "LH", sheet.getSheetName());
        } catch (Exception e) {
            logger.info("Error Occured in createPictureForHeader : {}", e.getMessage());
        }
    }

    private void createPictureForHeader(XSSFSheet sheet, int pictureIdx, String pictureTitle, int vmlIdx, String headerPos, String sheetName) {
        try {
            OPCPackage opcpackage = sheet.getWorkbook().getPackage();
            // creating /xl/drawings/vmlDrawing1.vml
            String partSheetName = new StringBuilder("/xl/drawings/vmlDrawing").append(vmlIdx)
                    .append(sheetName.replaceAll("\\s", "")).append(".vml").toString();
            PackagePartName partname = PackagingURIHelper.createPartName(partSheetName);
            PackagePart part = opcpackage.createPart(partname, "application/vnd.openxmlformats-officedocument.vmlDrawing");
            // creating new VmlDrawing
            VmlDrawing vmldrawing = new VmlDrawing(part);
            // creating the relation to the picture in
            // /xl/drawings/_rels/vmlDrawing1.vml.rels
            XSSFPictureData picData = sheet.getWorkbook().getAllPictures().get(pictureIdx);
            String rIdPic = vmldrawing.addRelation(null, XSSFRelation.IMAGES, picData).getRelationship().getId();
            // get image dimension
            ByteArrayInputStream is = new ByteArrayInputStream(picData.getData());
            // setting the image width = 3cm and height = 1.5 cm in pixels
            java.awt.Dimension imageDimension = new java.awt.Dimension(162, 56);
            is.close();
            // updating the VmlDrawing
            vmldrawing.setRelationIdPic(rIdPic);
            vmldrawing.setPictureTitle(pictureTitle);
            vmldrawing.setImageDimension(imageDimension);
            vmldrawing.setHeaderPosition(headerPos);
            // creating the relation to /xl/drawings/vmlDrawing1.xml in
            String rIdExtLink = sheet.getWorkbook().getSheet(sheetName).addRelation(null, XSSFRelation.VML_DRAWINGS, vmldrawing).getRelationship().getId();
            sheet.getWorkbook().getSheet(sheetName).getCTWorksheet().addNewLegacyDrawingHF().setId(rIdExtLink);
        } catch (Exception e) {
            logger.info("Error Occured in createPictureForHeader : {}", e.getMessage());
        }
    }

    @Override
    public PdfWriter addPdfHeaderAndFooter(PdfWriter writer) {
        PdfHeaderFooterPageEvent event = new PdfHeaderFooterPageEvent(Constants.HEADER_Y_POSITION, Constants.IMAGE_X_POSITION, Constants.IMAGE_Y_POSITION);
        writer.setPageEvent(event);
        return writer;
   }

	@Override
	public List<LookUp> getAllLookUpWindowDetails() {
		List<LookupWindow> lookupWindows = commonDao.getAllLookUpWindows();
		return setLookUpDatas(lookupWindows);
	}

	private List<LookUp> setLookUpDatas(List<LookupWindow> lookupWindows) {
		List<LookUp> lookups = new ArrayList<>();
		lookupWindows.forEach(lookupWindow -> {
			LookUp lookup = new LookUp();
			if (lookupWindow.getDataTypeCode().equals("9")) {
				List<String> userLookups = commonDao.getUserDeffinedLookup();
				userLookups.forEach(userLookup -> {
					LookUp userDefinedLookup = new LookUp();
					userDefinedLookup.setCode(lookupWindow.getTableName() + "#" + userLookup);
					userDefinedLookup.setDescription(userLookup);
					userDefinedLookup.setDataType(lookupWindow.getDataTypeCode());
					lookups.add(userDefinedLookup);
				});
			} else if (lookupWindow.getDataTypeCode().equals("8")) {
				lookup.setCode(lookupWindow.getTableName() + "#" + lookupWindow.getColumnName());
				lookup.setDescription(lookupWindow.getDescription());
				lookup.setDataType(lookupWindow.getDataTypeCode());
				lookups.add(lookup);
			} else {
				lookup.setCode(lookupWindow.getColumnName());
				lookup.setDescription(lookupWindow.getDescription());
				lookup.setDataType(lookupWindow.getDataTypeCode());
				lookups.add(lookup);
			}
		});
		return lookups;
    }

    @Override
	public List<Role> findRole(String searchString) {
		return commonDao.findRole(searchString);
	}

	@Override
	public String getSponsorFormatBySponsorDetail(String sponsorCode, String sponsorName, String sponsorAcronym) {
		String sponsorFormat = Constants.SPONSOR_FORMAT;
		if (sponsorCode != null) {
			sponsorFormat = sponsorFormat.replace("SPONSOR_CODE", sponsorCode);
		} else {
			sponsorFormat = sponsorFormat.replace("SPONSOR_CODE", "");
		}
		if (sponsorName != null) {
			sponsorFormat = sponsorFormat.replace("SPONSOR_NAME", sponsorName);
		} else {
			sponsorFormat = sponsorFormat.replace("SPONSOR_NAME", "");
		}
		if (sponsorAcronym != null) {
			sponsorFormat = sponsorFormat.replace("SPONSOR_ACRONYM", sponsorAcronym);
		} else {
			sponsorFormat = sponsorFormat.replace("(SPONSOR_ACRONYM)", "");
		}
		return sponsorFormat;
	}

	@Override
	public boolean getWebSocketConfigurationValue(String configKey) {
		try {
			WebSocketConfiguration socketConfiguration = commonDao.getWebSocketConfigurationValue(configKey);
			String value = socketConfiguration != null ? socketConfiguration.getConfigurationValue() : null;
			if (value == null) {
				logger.info("web socket config value is not set or empty. key is : {}", configKey);
				value = "false";
			}
			return Truth.strToBooleanIgnoreCase(value);
		} catch (Exception e) {
			logger.error("error occured in getWebSocketConfigurationValue for key : {}", configKey);
			return false;
		}
	}

	@Override
	public String personCertificationMailLog(CommonVO commonVO) {
		List<Object[]> notificationLogList = null;
		notificationLogList = commonDao.getNotificationLogForPerson(Constants.DEV_PROPOSAL_MODULE_CODE, 
				commonVO.getModuleItemKey(), commonVO.getProperty1(), Constants.NOTIFICATION_PROPOSAL_PERSONS);
		List<Map<String, Object>> finalResult= new ArrayList<>();
		notificationLogList.forEach(log -> {
			Map<String, Object> notification = new HashMap<>();
			notification.put("sendDate", log[0]);
			notification.put("mailSentFlag", log[1]);
			notification.put("errorMsg", log[2]);
			finalResult.add(notification);
		});
		return commonDao.convertObjectToJSON(finalResult);
	}

	@Override
	public List<Country> getCountryLookUp() {
		return commonDao.getCountryLookUp();
	}

    @Override
    public String getUnitFormatByUnitDetail(String unitNumber, String unitName) {
        String unitFormat = Constants.UNIT_FORMAT;
        if (unitNumber != null) {
            unitFormat = unitFormat.replace("UNIT_NUMBER", unitNumber);
        } else {
            unitFormat = unitFormat.replace("UNIT_NUMBER", "");
        }
        if (unitName != null) {
            unitFormat = unitFormat.replace("UNIT_NAME", unitName);
        } else {
            unitFormat = unitFormat.replace("UNIT_NAME", "");
        }
        return unitFormat;
    }

	@Override
	public String getPlaceHolderDataForRouting(Integer approvalStopNumber, Integer mapId, Integer workflowdetailId) {
		String stopName = null;
        if (approvalStopNumber != null && mapId != null) {
			stopName = workflowDao.fetchStopNameBasedMapIdAndStop(mapId, approvalStopNumber);
		} else if (workflowdetailId != null) {
			 WorkflowDetail workFlowDetails = workflowDao.getWorkFlowDetails(workflowdetailId);
				if (workFlowDetails.getMapId() != null && workFlowDetails.getApprovalStopNumber() != null) {
					stopName = workflowDao.fetchStopNameBasedMapIdAndStop(workFlowDetails.getMapId(),
							workFlowDetails.getApprovalStopNumber());
					approvalStopNumber =  workFlowDetails.getApprovalStopNumber();
				}
		}
		if (approvalStopNumber != null && (stopName == null || "".equals(stopName))) {
			stopName = "Stop " + approvalStopNumber;
		}
		return stopName;
	}

    @Override
    public ResponseEntity<Object> getAllLetterTemplateTypes(CommonVO vo) {
        List<LetterTemplateType> letterTemplateTypes = new ArrayList<>();
        commonDao.fetchAllLetterTemplateTypes(vo.getSearchString()).forEach(obj -> {
            Object[] tempObj = (Object[]) obj;
            LetterTemplateType letterTemplateType = new LetterTemplateType();
            letterTemplateType.setLetterTemplateTypeCode((String) tempObj[0]);
            letterTemplateType.setFileName((String) tempObj[1]);
            letterTemplateType.setContentType((String) tempObj[2]);
            letterTemplateType.setPrintFileType((String) tempObj[3]);
            letterTemplateTypes.add(letterTemplateType);
        });
        return new ResponseEntity<>(letterTemplateTypes, HttpStatus.OK);
    }


	@Override
    public String hashBySha(Object valueToHide) throws GeneralSecurityException {
        if (valueToHide != null && !StringUtils.isEmpty(valueToHide.toString())) {
            try {
                MessageDigest md = MessageDigest.getInstance(Constants.HASH_ALGORITHM_SHA);
                return new String(Base64.encodeBase64(md.digest(valueToHide.toString().getBytes(Constants.CHARSET))),
                        Constants.CHARSET);
            } catch (UnsupportedEncodingException arg2) {
                return "";
            }
        } else {
            return "";
        }
    }

	@Override
    public Boolean checkSFTPConnection(EmailContent emailContent) {
        boolean connectionStatus = true;
        com.jcraft.jsch.Session session = null;
        Channel channel = null;
        ChannelSftp channelSftp = null;
        try {
            String SFTPHOST = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_HOST);
            int SFTPPORT = Integer.parseInt(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PORT));
            String SFTPUSER = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_USER);
            logger.info("preparing the host information for sftp connection check.");
            JSch jsch = new JSch();
            session = jsch.getSession(SFTPUSER, SFTPHOST, SFTPPORT);
            session.setPassword(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PASSWORD));
            java.util.Properties config = new java.util.Properties();
            config.put("StrictHostKeyChecking", "no");
            session.setConfig(config);
            session.connect();
            logger.info("Host connected.");
            channel = session.openChannel("sftp");
            channel.connect();
            if(!channel.isConnected()) {
                connectionStatus = false;
                emailContent.getError().append("SFTP connection failed ").append("<br/>");
            }
        } catch (Exception ex) {
            logger.info("SFTP connection failed");
            connectionStatus = false;
            logger.info("Exception while trying to check the sftp connection {}", ex.getMessage());
            emailContent.getError().append("Exception while trying to check the sftp connection {} : ").append(ex).append("<br/>");
        } finally {
            logger.info("Initiating SFTP Host final disconnection");
            if(channelSftp != null)
                channelSftp.exit();
            logger.info("sftp Channel exited.");
            if(channel != null)
                channel.disconnect();
            logger.info("Channel disconnected.");
            if(session != null)
                session.disconnect();
            logger.info("Host Session disconnected.");
        }
        return connectionStatus;
    }

}
