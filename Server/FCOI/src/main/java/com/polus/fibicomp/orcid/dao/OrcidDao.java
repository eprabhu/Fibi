package com.polus.fibicomp.orcid.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.orcid.pojo.AwardPersonOrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidErrorLog;
import com.polus.fibicomp.orcid.pojo.OrcidWebhookNotificationLog;
import com.polus.fibicomp.orcid.pojo.OrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidWorkCategory;
import com.polus.fibicomp.orcid.pojo.OrcidWorkType;
import com.polus.fibicomp.orcid.pojo.PersonOrcidWork;
import com.polus.fibicomp.person.pojo.Person;

@Transactional
@Service
public interface OrcidDao {

	public List<String> getAllOrcidIds();

	/**
	 * This method is used to save or update an orcidWork
	 * @param work - Object of a orcidWork.
	 * @return An object of orcidWork.
	 */
	public OrcidWork saveOrUpdateOrcidWork(OrcidWork orcidWork);

	/**
	 * This method is used to fetch person orcid works by personId
	 * @param personId
	 * @return A List of persoOrcidWorks.
	 */
	public List<PersonOrcidWork> fetchPersonOrcidWorksBypersonId(String personId);

	/**
	 * This method is used to get orcid work by putCode
	 * @param putCode
	 * @return An object of orcidWork.
	 */
	public OrcidWork getOrcidWorkById(Integer putCode);

	/**
	 * This method is used to get orcid work category code by workTypeCode
	 * @param type
	 * @return orcidWorkCategory.
	 */
	public String getOrcidWorkCategoryCodeByTypeCode(String type);

	/**
	 * This method is used to save or update awardpersonorcidwork
	 * @param awardPersonOrcidWork
	 * @return An object of AwardPersonOrcidWork .
	 */
	public AwardPersonOrcidWork saveOrUpdateAwardPersonOrcidWork(AwardPersonOrcidWork orcidWork);

	/**
	 * This method is used to get all orcid work categories
	 * @return A list of OrcidWorkCategory.
	 */
	public List<OrcidWorkCategory> getAllOrcidWorkCateogories();

	/**
	 * This method is used to get all orcid work types
	 * @return A list of OrcidWorkType.
	 */
	public List<OrcidWorkType> getAllOrcidWorkTypes();

	/**
	 * This method is used to delete orcid work contributor
	 * @param putCode
	 */
	public void deleteOrcidWorkConributor(Integer putCode);

	/**
	 * This method is used to delete orcid work identifier
	 * @param putCode
	 */
	public void deleteOrcidWorkIdentifier(Integer putCode);

	/**
	 * This method is used to get country name by country two code
	 * @param CountryName
	 */
	public String getCountryNameByCountryTwoCode(String countryCode);

	/**
	 * This method is used to save Or Update Person OrcidWork
	 * @param personOrcidWork - Object of a PersonOrcidWork.
	 * @return An object of PersonOrcidWork.
	 */
	public PersonOrcidWork saveOrUpdatePersonOrcidWork(PersonOrcidWork personOrcidWork);

	/**
	 * This method is used to check if Put Code is already Found
	 * @param putCode
	 * @param orcidId
	 * @return Boolean
	 */
	public Boolean isPutCodeAlreadyFound(Integer putCode, String orcidId);

	/**
	 * This method is used to get all orcid webhook notification logs
	 * @return An list of OrcidWebhookNotificationLog.
	 */
	public List<OrcidWebhookNotificationLog> getAllOrcidWebhookNotificationLog();

	/**
	 * This method is used to get person detail by orcidId
	 * @param orcidId
	 * @return Person
	 */
	public Person getPersonDetailByOrcidId(String orcidId);

	/**
	 * This method is used to delete all orcid webhook notification logs
	 * @return An list of OrcidWebhookNotificationLog.
	 */
	public void deleteAllOrcidWebhookNotificationLog(List<OrcidWebhookNotificationLog> orcidWebhookNotificationLogs);

	/**
	 * This method is used to save Or Update OrcidWebhookNotificationLog
	 * @param orcidWebhookNotificationLog - Object of a OrcidWebhookNotificationLog.
	 * @return An object of OrcidWebhookNotificationLog.
	 */
	public void saveOrUpdateOrcidWebhookNotificationLog(OrcidWebhookNotificationLog orcidWebhookNotificationLog);

	/**
	 * This method is used to get AwardPersonOrcidWorks by personOrcidWorkId
	 * @param personOrcidWorkId
	 * @return An list of AwardPersonOrcidWork.
	 */
	public List<AwardPersonOrcidWork> getAwardPersonOrcidWorksByPersonOrcidWorkId(Integer personOrcidWorkId);

	/**
	 * This method is used to get personId by orcidId
	 * @param orcidId
	 * @return personId
	 */
	public Person getPersonIdByOrcidId(String orcidId);

	/**
	 * This method is used to delete OrcidWebhookNotificationLog
	 * @param orcidWebhookNotificationLog - object of OrcidWebhookNotificationLog
	 */
	public void deleteOrcidWebhookNotificationLog(OrcidWebhookNotificationLog orcidWebhookNotificationLog);

	/**
	 * This method is used to get AwardPersonOrcidWork by awardPersonOrcidWorkId
	 * @param awardPersonOrcidWorkId
	 * @return AwardPersonOrcidWork
	 */
	public AwardPersonOrcidWork getAwardPersonOrcidWorkById(Integer awardPersonOrcidWorkId);

	/**
	 * This method is used to delete AwardPersonOrcidWork
	 * @param orcidWork - object of AwardPersonOrcidWork
	 */
	public void deleteAwardPersonOrcidWork(AwardPersonOrcidWork orcidWork);

	/**
	 * This method is used to get AwardPersonOrcidWorks by awardNumber
	 * @param awardNumber
	 * @return list of AwardPersonOrcidWork
	 */
	public List<AwardPersonOrcidWork> getAwardPersonOrcidWorksByAwardNumber(String awardNumber);

	/**
	 * This method is used to save Or Update OrcidErrorLog
	 * @param orcidErrorLog - Object of a OrcidErrorLog.
	 */
	public void saveOrUpdateOrcidErrorLog(OrcidErrorLog orcidErrorLog);

	/**
	 * This method is used to get all put codes by orcidId
	 * @param orcidId
	 * @return list of putcodes
	 */
	public List<Integer> getAllPutCodesBasedOnOrcidId(String orcidId);

}
