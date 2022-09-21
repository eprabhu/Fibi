package com.polus.fibicomp.award.awardreviewcomment.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.awardreviewcomment.dao.AwardReviewCommentDao;
import com.polus.fibicomp.award.awardreviewcomment.dto.AwardReviewCommentDto;
import com.polus.fibicomp.award.awardreviewcomment.vo.AwardReviewCommentVO;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardReviewComment;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.workflow.dao.WorkflowDao;

@Transactional
@Configuration
@Service(value = "awardReviewCommentsService")
public class AwardReviewCommentServiceImpl implements AwardReviewCommentService {

	protected static Logger logger = LogManager.getLogger(AwardReviewCommentServiceImpl.class.getName());

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public AwardReviewCommentDao awardReviewCommentDao;

	@Autowired
	public PersonDao personDao;

	@Autowired
	public WorkflowDao workflowDao;

	@Autowired
	public AwardDao awardDao;

	@Override
	public String saveOrUpdateAwardReviewComment(AwardReviewCommentVO vo) {
		AwardReviewComment awardReviewComment = vo.getAwardReviewComment();
		if (awardReviewComment.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_ACTIVE)) {
			List<String> awardSequenceStatuses = new ArrayList<>();
			awardSequenceStatuses.add(Constants.AWARD_FINAL_STATUS_PENDING);
			Award pendingAward = awardDao.fetchAwardByAwardNumberAndAwardSequenceStatus(awardReviewComment.getAwardNumber(), awardSequenceStatuses);
			if (pendingAward != null && pendingAward.getAwardId() != null && awardReviewComment.getAwardReviewCommentId() == null) {
				awardReviewComment.setAwardId(pendingAward.getAwardId());
				awardReviewComment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardReviewCommentDao.saveOrUpdateAwardReviewComment(awardReviewComment);
			}
		}
		awardReviewComment.setAwardId(vo.getAwardId());
		awardReviewComment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardReviewCommentDao.saveOrUpdateAwardReviewComment(awardReviewComment);
		awardReviewComment.setReviewerFullName(personDao.getPersonFullNameByPersonId(awardReviewComment.getReviewerPersonId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchAwardReviewCommentsByAwardId(AwardReviewCommentVO vo) {
		List<AwardReviewCommentDto> awardReviewCommentsHierarchy = new ArrayList<>();
		String leadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(vo.getAwardId());
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(vo.getPersonId(), Constants.VIEW_PRIVATE_COMMENTS_RIGHT, leadUnitNumber);
		if (vo.getAwardId() != null) {
			List<AwardReviewComment> awardReviewComments = awardReviewCommentDao.getAwardReviewCommentsByAwardId(vo.getAwardId(), isPersonHasPermission, vo.getLoginPersonUnitNumber(), vo.getAwardNumber());
			if (awardReviewComments != null && !awardReviewComments.isEmpty()) {
				Set<String> personId = awardReviewComments.stream().map(AwardReviewComment::getReviewerPersonId).collect(Collectors.toSet());
				personId.addAll(awardReviewComments.stream().map(AwardReviewComment::getResolvedBy).collect(Collectors.toSet()));
				if(!personId.isEmpty()) {
					List<Person> personDetails = commonDao.getPersonDetailByPersonId(new ArrayList<>(personId));
					Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(Person::getPersonId, Person::getFullName));
					awardReviewComments.stream()
					   .filter(awardReviewComment -> collect.containsKey(awardReviewComment.getResolvedBy()))
					   .forEach(awardReviewComment -> awardReviewComment.setResolvedByFullName(collect.get(awardReviewComment.getResolvedBy())));
					awardReviewComments.stream()
					   .filter(awardReviewComment -> collect.containsKey(awardReviewComment.getReviewerPersonId()))
					   .forEach(awardReviewComment -> awardReviewComment.setReviewerFullName(collect.get(awardReviewComment.getReviewerPersonId())));
				}
				awardReviewComments.stream().forEach(awardReviewComment -> {
					AwardReviewCommentDto awardReviewCommentDto = setAwardReviewCommentDtoDetails(awardReviewComment);
					awardReviewCommentsHierarchy.add(awardReviewCommentDto);
				});
				//Reply to comment functionality is commented on front end
				//awardReviewCommentsHierarchy = buildReviewCommentsHierarchy(awardReviewCommentsHierarchy);
			}
		}	
		vo.setAwardReviewComments(awardReviewCommentsHierarchy);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String resolveAwardReviewComment(AwardReviewCommentVO vo) {
		AwardReviewComment awardReviewComment = awardReviewCommentDao.getAwardReviewCommentById(vo.getAwardReviewCommentId());
		awardReviewComment.setIsResolved(true);
		awardReviewComment.setResolvedBy(vo.getPersonId());
		awardReviewComment.setResolvedTimeStamp(commonDao.getCurrentTimestamp());
		awardReviewComment = awardReviewCommentDao.saveOrUpdateAwardReviewComment(awardReviewComment);
		awardReviewComment.setReviewerFullName(personDao.getPersonFullNameByPersonId(awardReviewComment.getReviewerPersonId()));
		awardReviewComment.setResolvedByFullName(personDao.getPersonFullNameByPersonId(awardReviewComment.getResolvedBy()));
		vo.setAwardReviewComment(awardReviewComment);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getListOfAwardReviewPersons(AwardReviewCommentVO vo) {
		List<PersonDTO> persons = new ArrayList<>();
		Set<String> personIds = new HashSet<>();
		String awardLeadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(vo.getAwardId());
		List<PersonRoleRT> personRoleRTs = personDao.fetchPersonRoleRTByRightNameAndUnitNumber(awardLeadUnitNumber,Constants.MAINTAIN_REVIEW_COMMENTS_RIGHT_NAME);
		if (personRoleRTs != null && !personRoleRTs.isEmpty()) {
			for (PersonRoleRT personRoleRT : personRoleRTs) {
				personIds.add(personRoleRT.getPersonRoleRTAttributes().getPersonId());
			}
		}
		personIds.addAll(awardReviewCommentDao.getReviewerPersonIdAndFullNameByAwardId(vo.getAwardId()));
		personIds.addAll(workflowDao.getLatestWorkflowPersonIdsByParams(vo.getAwardId().toString(),Constants.MODULE_CODE_AWARD));
		for (String personId : personIds) {
			PersonDTO person = new PersonDTO();
			person.setPersonID(personId);
			person.setFullName(personDao.getPersonFullNameByPersonId(personId));
			persons.add(person);
		}
		vo.setPersons(persons);
		return commonDao.convertObjectToJSON(vo);
	}

	public List<AwardReviewCommentDto> buildReviewCommentsHierarchy(List<AwardReviewCommentDto> reviewComments) {
		List<AwardReviewCommentDto> commentsHierarchy = new ArrayList<>();
		for (AwardReviewCommentDto comment : reviewComments) {
			if (comment.getParentReviewCommentId() == null) {
				commentsHierarchy.add(comment);
			}
		}
		getReviewCommentsHierarchy(commentsHierarchy, reviewComments);
		return commentsHierarchy;
	}

	private AwardReviewCommentDto setAwardReviewCommentDtoDetails(AwardReviewComment awardReviewComment) {
		AwardReviewCommentDto awardReviewCommentDto = new AwardReviewCommentDto();
		awardReviewCommentDto.setAwardReviewCommentId(awardReviewComment.getAwardReviewCommentId());
		awardReviewCommentDto.setParentReviewCommentId(awardReviewComment.getParentReviewCommentId());
		awardReviewCommentDto.setAwardId(awardReviewComment.getAwardId());
		awardReviewCommentDto.setAwardNumber(awardReviewComment.getAwardNumber());
		awardReviewCommentDto.setSequenceNumber(awardReviewComment.getSequenceNumber());
		awardReviewCommentDto.setReviewComment(awardReviewComment.getReviewComment());
		awardReviewCommentDto.setReviewSectionCode(awardReviewComment.getReviewSectionCode());
		awardReviewCommentDto.setReviewCommentTypeCode(awardReviewComment.getReviewCommentTypeCode());
		awardReviewCommentDto.setIsPrivateComment(awardReviewComment.getIsPrivateComment());
		awardReviewCommentDto.setReviewerPersonId(awardReviewComment.getReviewerPersonId());
		awardReviewCommentDto.setIsResolved(awardReviewComment.getIsResolved());
		awardReviewCommentDto.setResolvedBy(awardReviewComment.getResolvedBy());
		awardReviewCommentDto.setUpdateTimeStamp(awardReviewComment.getUpdateTimeStamp());
		awardReviewCommentDto.setUpdateUser(awardReviewComment.getUpdateUser());
		awardReviewCommentDto.setReviewerFullName(awardReviewComment.getReviewerFullName());
		if (awardReviewComment.getResolvedByFullName() != null) {
			awardReviewCommentDto.setResolvedByFullName(awardReviewComment.getResolvedByFullName());
			awardReviewCommentDto.setResolvedTimeStamp(awardReviewComment.getResolvedTimeStamp());
		}
		return awardReviewCommentDto;
	}

	private void getReviewCommentsHierarchy(List<AwardReviewCommentDto> commentsHierarchy, List<AwardReviewCommentDto> reviewComments) {
		if (commentsHierarchy != null && !commentsHierarchy.isEmpty()) {
			for (Integer i = 0; i < commentsHierarchy.size(); i++) {
				AwardReviewCommentDto comment = commentsHierarchy.get(i);
				for (Integer j = 0; j < reviewComments.size(); j++) {
					AwardReviewCommentDto reply = reviewComments.get(j);
					if (reply.getParentReviewCommentId() != null && reply.getParentReviewCommentId().equals(comment.getAwardReviewCommentId())) {
						comment.getReplies().add(reply);
					}
				}
				getReviewCommentsHierarchy(comment.getReplies(), reviewComments);
			}
		}
	}

	@Override
	public String deleteAwardReviewComment(AwardReviewCommentVO vo) {
		List<Integer> allAwardReviewCommentIds = new ArrayList<>();
		allAwardReviewCommentIds.add(vo.getAwardReviewCommentId());
		String deleteMessage = deleteChildComments(allAwardReviewCommentIds);
		vo.setMessage(deleteMessage);
		return commonDao.convertObjectToJSON(vo);
	}

	private String deleteChildComments(List<Integer> allAwardReviewCommentIds) {
		String deleteMessage = "";
		if (allAwardReviewCommentIds != null && !allAwardReviewCommentIds.isEmpty()) {
			List<Integer> childReviewCommentIds = new ArrayList<>();
			for (Integer reviewCommentId : allAwardReviewCommentIds) {
				childReviewCommentIds.addAll(awardReviewCommentDao.getChildAwardReviewCommentIdsByAwardReviewCommentId(reviewCommentId));
				deleteMessage = awardReviewCommentDao.deleteAwardReviewComment(reviewCommentId);
			}
			deleteChildComments(childReviewCommentIds);
		}
		return deleteMessage;
	}

}
