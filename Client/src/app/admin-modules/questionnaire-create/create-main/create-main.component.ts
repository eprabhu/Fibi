import { Component, OnInit, ChangeDetectionStrategy, OnDestroy, ChangeDetectorRef, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import * as _ from 'lodash';
import { CreateQuestionnaireService } from '../services/create.service';
import { Subscription } from 'rxjs';
import { BasicDetailsComponent } from './basic-details/basic-details.component';
import { CommonService } from '../../../common/services/common.service';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';

/**
 * refer maintenance/questionnaire-list component for understanding the query param handling
 * and what is it purpose
 */
@Component({
  selector: 'app-create-main',
  templateUrl: './create-main.component.html',
  styleUrls: ['./create-main.component.css'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class CreateMainComponent implements OnInit, OnDestroy {

  constructor(private _activatedRoute: ActivatedRoute, private _commonService: CommonService,
    private _createQuestionnaireService: CreateQuestionnaireService,
    private _changeRef: ChangeDetectorRef,
    private _router: Router) { }
  @ViewChild(BasicDetailsComponent, { static: false }) basicDetailsComponent: BasicDetailsComponent;
  data: any = {};
  toast_message = 'Questionnaire saved successfully.';
  QuestionnaireCommonValues: any = {
    lastQuestionId: 1,
    lastGroupName: 1,
    lastConditionId: 1,
    isQuestionEdited: false
  };
  isSaving = false;
  isEnableVersion = false;
  currentTab = 'basic';
  nodes: any = {
    nodes: []
  };
  errorList = [];
  groupLabels = {};
  isViewMode: any;
  editIndex: any;
  $subscriptions: Subscription[] = [];
  /**
   * takes the data from the resolver output,
   * updates the lastGroupName with max value to the current questionnaire group number;
   */
  ngOnInit() {
    this.data = this._activatedRoute.snapshot.data['Questionnaire'];
    this.QuestionnaireCommonValues.lastGroupName = this.data.questionnaire.maxGroupNumber + 1 || 1;
    this.data.questionnaire_id ? this.currentTab = 'create' : this.currentTab = 'basic';
    this.goToQuestion();
    this.$subscriptions.push(this._activatedRoute.queryParams.subscribe((data: any) => {
      this.isViewMode = (data.id && data.id.slice(0, 1) === 'T');
      this.isEnableVersion = (data.id && data.id.slice(1, 2) === 'T');
      if (this.isEnableVersion) {
        this.data.header.IS_FINAL = false;
      }
    }));
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  changeCurrentTab(selectedTab) {
    this.currentTab = selectedTab;
  }
  validateQuestionnaire() {
    let isQuestionnaireValid = true;
    this.errorList = [];
    _.forEach(this.data.questionnaire.questions, (question) => {
      if (!question.QUESTION)  {
        this.errorList.push(question.QUESTION_ID);
      } else if (['elastic', 'endpoint'].includes(question.ANSWER_TYPE) &&
         (!question.LOOKUP_TYPE || question.LOOKUP_TYPE == 'null' || !question.LOOKUP_NAME || question.LOOKUP_NAME == 'null')) {
        this.errorList.push(question.QUESTION_ID);
      } else if (['SystemLookup', 'UserLookup'].includes(question.ANSWER_TYPE) &&
                (!question.LOOKUP_TYPE || question.LOOKUP_TYPE == 'null')) {
        this.errorList.push(question.QUESTION_ID);
      }
    });
    _.forEach(this.data.questionnaire.options, (option) => {
      if (option.OPTION_LABEL === '' || option.OPTION_LABEL == null) {
        this.errorList.push(option.QUESTION_ID);
      }
    });
    _.forEach(this.data.questionnaire.conditions, (condition) => {
      if (condition.CONDITION_VALUE === '' || condition.CONDITION_VALUE == null) {
        this.errorList.push(condition.QUESTION_ID);
      }
    });
    if (this.data.header.QUESTIONNAIRE_NAME == null && this.data.header.QUESTIONNAIRE_NAME === '') {
      isQuestionnaireValid = false;
    }
    if (this.errorList.length && isQuestionnaireValid) {
      this.errorList.forEach((error: any) => {
        document.getElementById('tree' + error).classList.add('highlight-error-node');
      });
    }
    else {
      this.saveQuestionnaire();
    }
  }
  confirmSave() {
    if (!this.isSaving) {
      this.data.questionEditted = this.QuestionnaireCommonValues.isQuestionEdited;
        if (this.data.header.QUESTIONNAIRE_NAME) {
          if (this.data.questionnaire.questions.length !== 0) {
            document.getElementById('saveQuestionnaire').click();
          } else {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Please add at least one question');
          }
        } else {
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Please enter a valid name ');
        }
    }
  }
  /**
   * saves the questionnaire
   */
  saveQuestionnaire() {
    if (!this.isSaving) {
      if (this.basicDetailsComponent) {
        this.basicDetailsComponent.addOrUpdate(false);
      }
      this.updateGroupLabel();
      this.setQuestionnaireVersion();
      this.isSaving = true;
      this.data.header.UPDATE_TIMESTAMP =  new Date().getTime();
      this.data.header.UPDATE_USER = this._commonService.getCurrentUserDetail('userName');
      this.$subscriptions.push(this._createQuestionnaireService.saveQuestionnaireList(this.data).subscribe(
        data => {
          this.data = data;
          if (this.data.questionEditted) {
            this.data.questionEditted = false;
            this.updateQuestionnaireId(this.data.questionnaireId);
          }
          this.data.questionnaire.deleteList.condition = [];
          this.data.questionnaire.deleteList.option = [];
          this.data.questionnaire.deleteList.question = [];
          this._changeRef.markForCheck();
          this._commonService.showToast(HTTP_SUCCESS_STATUS, this.toast_message);
          this.isSaving = false;
        }, err => this.isSaving = false
      ));
    }
  }

  updateQuestionnaireId(questionnaireId: number = null , viewMode = 'F', version = 'F') {
    this._router.navigate(['../create'],
      { queryParams: { id: this.appendValuesToQueryParams(questionnaireId, viewMode, version) }, relativeTo: this._activatedRoute });
  }

  appendValuesToQueryParams(questionnaireId: number, viewMode = 'F', version = 'F') {
    return questionnaireId ? viewMode + version + questionnaireId : null;
  }
  /**
   * updates the group label of questionnaire with user updated values or group name itself
   */
  updateGroupLabel() {
    _.forEach(this.data.questionnaire.questions, (question) => {
      question.GROUP_LABEL = this.groupLabels[question.GROUP_NAME];
    });
  }

  updateGroupname() {
    _.forEach(this.data.questionnaire.questions, (question, key) => {
      this.groupLabels[question.GROUP_NAME] = question.GROUP_LABEL || question.GROUP_NAME;
    });
  }

  addNewQuestion(groupName) {
    this._createQuestionnaireService.addQuestionEvent.next(groupName);
  }
  goToQuestion() {
    this.$subscriptions.push(this._createQuestionnaireService.updateSelectedQuestionId
      .subscribe(data => {
        if (this.currentTab !== 'create') {
          this.currentTab = 'create';
          setTimeout(() => {
            this._createQuestionnaireService.updateSelectedQuestionId.next(data);
          }, 250);
        }
      }));
  }
  /**
   * enables the new_questionnaire_version as true if the questionnaire is already answered and opened in edit mode
   * which will enable a new version
   */
  setQuestionnaireVersion() {
    this.data.newQuestionnaireVersion = (this.isEnableVersion && this.data.questionEditted);
  }
}
