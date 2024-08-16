
package com.polus.fibicomp.coi.service;

import static java.util.stream.Collectors.groupingBy;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dao.ProjectDao;
import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.dto.ProjectCommentDto;
import com.polus.fibicomp.coi.dto.ProjectOverviewDto;
import com.polus.fibicomp.coi.dto.ProjectOverviewResponseDto;
import com.polus.fibicomp.coi.pojo.CoiProjectComment;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;

@Service
@Transactional
public class ProjectServiceImpl implements ProjectService {

	protected static Logger logger = LogManager.getLogger(ProjectServiceImpl.class.getName());

	private static final String DISCLOSURE_NOT_REQUIRED = "Not Required";
	private static final String DISCLOSURE_COMPLETED = "Completed";
	private static final String DISCLOSURE_PENDING = "Pending";
	private static final String PROJECT_TYPE_PROPOSAL = "Proposal";

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ProjectDao projectDao;

	@Autowired
	private PersonDao personDao;

	@Override
	public ResponseEntity<Object> saveComment(ProjectCommentDto dto) {
		CoiProjectComment coiProjectComment = CoiProjectComment.builder().comment(dto.getComment())
				.commentBy(AuthenticatedUser.getLoginPersonId()).commentTypeCode(dto.getCommentTypeCode())
				.isPrivate(dto.getIsPrivate() != null ? dto.getIsPrivate() : Boolean.FALSE)
				.moduleCode(dto.getModuleCode()).moduleItemKey(dto.getModuleItemKey())
				.parentCommentId(dto.getParentCommentId()).updatedBy(AuthenticatedUser.getLoginUserName())
				.updateTimestamp(commonDao.getCurrentTimestamp()).build();
		projectDao.saveComment(coiProjectComment);
		return new ResponseEntity<>("Comment saved successfully", HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> updateComment(ProjectCommentDto dto) {
		projectDao.updateComment(dto);
		return new ResponseEntity<>("Comment updated successfully", HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchComment(ProjectCommentDto dto) {
		List<CoiProjectComment> comments = projectDao.fetchComment(dto);
		List<ProjectCommentDto> commentsDto = mapCommentsToDto(comments);
		Map<Integer, List<ProjectCommentDto>> childComments = commentsDto.stream()
				.filter(comment -> comment.getParentCommentId() != null)
				.collect(groupingBy(ProjectCommentDto::getParentCommentId));
		commentsDto.removeIf(comment -> comment.getParentCommentId() != null);
		commentsDto = commentsDto.stream().map(comment -> {
			comment.setChildComments(childComments.get(comment.getCommentId()));
			return comment;
		}).collect(Collectors.toList());
		return new ResponseEntity<>(commentsDto, HttpStatus.OK);
	}

	private List<ProjectCommentDto> mapCommentsToDto(List<CoiProjectComment> comments) {
		return comments.stream().map(this::mapToDto).collect(Collectors.toList());
	}

	private ProjectCommentDto mapToDto(CoiProjectComment comment) {
		ProjectCommentDto dto = new ProjectCommentDto();
		dto.setCommentId(comment.getCommentId());
		dto.setCommentBy(comment.getCommentBy());
		dto.setParentCommentId(comment.getParentCommentId());
		dto.setComment(comment.getComment());
		dto.setCommentTypeCode(comment.getCommentTypeCode());
		dto.setCommentType(comment.getCommentType());
		dto.setModuleCode(comment.getModuleCode());
		dto.setIsPrivate(comment.getIsPrivate());
		dto.setModuleItemKey(comment.getModuleItemKey());
		dto.setUpdateTimestamp(comment.getUpdateTimestamp());
		dto.setUpdatedBy(comment.getUpdatedBy());
		dto.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(comment.getCommentBy()));
		return dto;
	}

	@Override
	public ResponseEntity<Object> deleteComment(Integer commentId) {
		if (projectDao.canDeleteComment(commentId)) {
			projectDao.deleteComment(commentId);
			return new ResponseEntity<>("Comment deleted successfully", HttpStatus.OK);
		} else {
			return new ResponseEntity<>("Comment cannot be deleted", HttpStatus.METHOD_NOT_ALLOWED);
		}
	}

	@Override
	public ResponseEntity<Object> fetchDashbaord(CoiDashboardVO vo) {
		Integer proposalCount = null;
		Map<String, List<DisclosureProjectDto>> projectDetailsGroupedById = projectDao.fetchProjectOverview(vo).stream()
				.collect(Collectors.groupingBy(DisclosureProjectDto::getProjectId, LinkedHashMap::new,
						Collectors.toList()));
		List<ProjectOverviewDto> projectOverviewDto = projectDetailsGroupedById.entrySet().stream().map(entry -> {
			String disclosureSubmissionStatus = null;
			String disclosureReviewStatus = null;
			List<DisclosureProjectDto> keyPersonDetails = entry.getValue().parallelStream().map(project -> {
				DisclosureProjectDto.DisclosureProjectDtoBuilder dto = DisclosureProjectDto.builder()
						.keyPersonId(project.getKeyPersonId()).keyPersonName(project.getKeyPersonName())
						.keyPersonRole(project.getKeyPersonRole()).homeUnitName(project.getHomeUnitName())
						.homeUnitNumber(project.getHomeUnitNumber())
						.questionnaireCompleted(project.getQuestionnaireCompleted())
						.disclosureId(project.getDisclosureId());
				if (project.getQuestionnaireCompleted()) {
					if (Boolean.TRUE.equals(project.getDisclsoureNeeded())) {
						if (Boolean.TRUE.equals(project.getDisclosureSubmitted())) {
							dto.disclosureStatus(DISCLOSURE_COMPLETED);
						} else {
							dto.disclosureStatus(DISCLOSURE_PENDING);
						}
						dto.disclosureReviewStatus(project.getDisclosureReviewStatus());
					} else if (Boolean.FALSE.equals(project.getDisclsoureNeeded())) {
						dto.disclosureStatus(DISCLOSURE_NOT_REQUIRED);
					}
				}
				return dto.build();
			}).collect(Collectors.toList());
			if (keyPersonDetails.stream().allMatch(dto -> DISCLOSURE_NOT_REQUIRED.equals(dto.getDisclosureStatus()))
					|| keyPersonDetails.stream().anyMatch(dto -> dto.getDisclosureStatus() == null)) {
				disclosureSubmissionStatus = null;
			} else if (keyPersonDetails.stream()
					.filter(dto -> !DISCLOSURE_NOT_REQUIRED.equalsIgnoreCase(dto.getDisclosureStatus()))
					.allMatch(dto -> DISCLOSURE_COMPLETED.equals(dto.getDisclosureStatus()))) {
				disclosureSubmissionStatus = DISCLOSURE_COMPLETED;
			} else {
				disclosureSubmissionStatus = DISCLOSURE_PENDING;
			}
			if (disclosureSubmissionStatus == null) {
			    disclosureReviewStatus = null;
			} else if (DISCLOSURE_COMPLETED.equals(disclosureSubmissionStatus) || DISCLOSURE_PENDING.equals(disclosureSubmissionStatus)) {
				boolean allReviewsCompleted = keyPersonDetails.stream()
						.filter(dto -> !DISCLOSURE_NOT_REQUIRED.equals(dto.getDisclosureStatus()))
						.allMatch(dto -> DISCLOSURE_COMPLETED.equals(dto.getDisclosureReviewStatus()));
				disclosureReviewStatus = allReviewsCompleted ? DISCLOSURE_COMPLETED : DISCLOSURE_PENDING;
			}
			int completeCount = (int) keyPersonDetails.stream()
					.filter(dto -> DISCLOSURE_COMPLETED.equals(dto.getDisclosureReviewStatus())).count();
			int inCompletCount = (int) keyPersonDetails.stream()
					.filter(dto -> !DISCLOSURE_COMPLETED.equals(dto.getDisclosureReviewStatus())).count();
			DisclosureProjectDto project = entry.getValue().get(0);
			DisclosureProjectDto projectDetails = DisclosureProjectDto.builder().projectId(project.getProjectId())
					.leadUnitName(project.getLeadUnitName()).leadUnitNumber(project.getLeadUnitNumber())
					.sponsorCode(project.getSponsorCode()).sponsorName(project.getSponsorName())
					.primeSponsorCode(project.getPrimeSponsorCode()).primeSponsorName(project.getPrimeSponsorName())
					.title(project.getTitle()).projectStartDate(project.getProjectStartDate())
					.projectEndDate(project.getProjectEndDate()).projectStatus(project.getProjectStatus())
					.submissionStatus(disclosureSubmissionStatus).disclosureReviewStatus(disclosureReviewStatus)
					.completeCount(completeCount).inCompletCount(inCompletCount).projectType(project.getProjectType())
					.projectTypeCode(project.getProjectTypeCode()).projectBadgeColour(project.getProjectBadgeColour())
					.updateTimestamp(project.getUpdateTimestamp()).piName(project.getPiName())
					.commentCount(project.getCommentCount()).keyPersonCount(keyPersonDetails.size()).build();
			ProjectOverviewDto projectOverview = new ProjectOverviewDto();
			projectOverview.setKeyPersonDetails(keyPersonDetails);
			projectOverview.setProjectDetails(projectDetails);
			return projectOverview;
		}).collect(Collectors.toList());
		if (vo.getIsDownload()) {
			proposalCount = projectDao.fetchProjectOverviewCount(vo);
		}
		return new ResponseEntity<>(ProjectOverviewResponseDto.builder().proposalCount(proposalCount)
				.projectOverviewDetails(projectOverviewDto).build(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getProjectStatusLookup(String projectType) {
		if (projectType.equals(PROJECT_TYPE_PROPOSAL)) {
			return new ResponseEntity<>(projectDao.getProposalStatusLookup(), HttpStatus.OK);
		}
		return new ResponseEntity<>(new ArrayList<>(), HttpStatus.OK);
	}

}