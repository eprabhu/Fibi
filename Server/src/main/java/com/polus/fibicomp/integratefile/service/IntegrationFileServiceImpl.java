package com.polus.fibicomp.integratefile.service;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.integratefile.dao.IntegrationFileDao;
import com.polus.fibicomp.integratefile.pojo.PersonFeed;

@Transactional
@Service(value = "integrationFileService")
public class IntegrationFileServiceImpl implements IntegrationFileService {

	protected static Logger logger = LogManager.getLogger(IntegrationFileServiceImpl.class.getName());

	@Autowired
	private IntegrationFileDao integrationFileDao;

	@Value("${person.feed.api}")
	private String tmpPersonFeedAPI;

	@Autowired
	private CommonDao commonDao;

	@Override
	public void readFileData(String filePath) throws Exception {
		String strCurrentLine = null;
		BufferedReader objReader = null;
		String[] values = null;
		try {
			objReader = new BufferedReader(new FileReader(filePath));
			integrationFileDao.deleteAllPersonFeed();
			Integer counter = 0;
			while ((strCurrentLine = objReader.readLine()) != null) {
				values = strCurrentLine.split(",(?=([^\"]*\"[^\"]*\")*(?![^\"]*\"))");
				for (int i = 0; i < values.length; i++) {
					if (values[i].isEmpty()) {
						values[i] = null;
					}
				}
				if (counter != 0) {
					PersonFeed personFeed = new PersonFeed();
					personFeed.setPersonFeedId(values[0]);
					personFeed.setLastName(values[1]);
					personFeed.setFirstName(values[2]);
					personFeed.setUserName(values[3]);
					personFeed.setEmailAddress(values[4]);
					if (values[5] == null) {
						personFeed.setDegree(null);
					} else {
						personFeed.setDegree(values[5].replace("\"", ""));
					}
					if (values[6] == null) {
						personFeed.setOfficeLocation(null);
					} else {
						personFeed.setOfficeLocation(values[6].replace("\"", ""));
					}
					personFeed.setOfficePhone(values[7]);
					personFeed.setDirectoryDepartment(values[8]);
					if (values[9] == null) {
						personFeed.setPrimaryTitle(null);
					} else {
						personFeed.setPrimaryTitle(values[9].replace("\"", ""));
					}
					personFeed.setHomeUnit(values[10]);
					personFeed.setRelationship(values[11]);
					personFeed.setAddressLineOne(values[12]);
					personFeed.setCity(values[13]);
					personFeed.setZipCode(values[14]);
					personFeed.setStateProvince(values[15]);
					integrationFileDao.savePersonFeedDetails(personFeed);
				}
				counter++;
			}
			boolean isIntegratePerson = integrationFileDao.savePersonFeedDetailToPerson();
			if (isIntegratePerson) {
				logger.info("File integrated to person");
			} else {
				logger.info("File not integrated to person");
			}
			objReader.close();
		} catch (Exception e) {
			exceptionAppendToFile(e);
			e.printStackTrace();
		}
	}

	private void exceptionAppendToFile(Exception e) {
		try {
			Timestamp currentDate = commonDao.getCurrentTimestamp();
			Path path = Paths.get(tmpPersonFeedAPI).getParent();
			String filePath = path.toString() + "/FileIntegratedException.txt";
			BufferedWriter out = new BufferedWriter(new FileWriter(filePath, true));
			out.write(currentDate.toString() + "\r\n" + StringUtils.repeat("-", 100) + "\r\n");
			PrintWriter pWriter = new PrintWriter(out, true);
			e.printStackTrace(pWriter);
		} catch (Exception ie) {
			throw new RuntimeException("Could not write Exception to file", ie);
		}
	}

//	@Scheduled(cron = "${tmp.person.feed.api.schedule}")
	public void scheduleIntegrationFile() {
		try {
			readFileData(tmpPersonFeedAPI);
		} catch (Exception e) {
			e.printStackTrace();
			logger.info("failed" + e);
		}
	}

}
