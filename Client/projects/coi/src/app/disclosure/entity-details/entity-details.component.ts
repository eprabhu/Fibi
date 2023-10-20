import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';
import { hideModal, openModal, scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { NavigationService } from '../../common/services/navigation.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
  selector: 'app-entity-details',
  templateUrl: './entity-details.component.html',
  styleUrls: ['./entity-details.component.scss']
})

export class EntityDetailsComponent implements  OnInit, OnDestroy {
  @Input() entityId: any;
  isTriggeredFromSlider = false;
  $subscriptions: Subscription[] = [];
  isSwitchCurrentTab = false;
  deleteRelationshipEvent = null;
  @Output() closeAction: EventEmitter<boolean> = new EventEmitter<boolean>();
  questionnaireSection: any = '';
  currentRelationshipDetails: any;

  constructor(public entityDetailService: EntityDetailsService, private _route: ActivatedRoute, private _router: Router,
    private _commonService: CommonService, private _navigationService: NavigationService) {
    this.clearSfiNavBarStyle();
  }

  updateRelationshipDetails: any;
  isEditMode = false;
  entityDetails = {};

  ngOnInit() {
    this.isTriggeredFromSlider = this.checkForUrl();
    this.getQueryParams();
  }

  getQueryParams() {
    this.$subscriptions.push(this._route.queryParams.subscribe(params => {
      this.isEditMode = params['mode'] === 'edit';
      this.entityId = params['personEntityId'] || this.entityId;
    this.getSfiEntityDetails();
    }));
  }

  checkForUrl() {
   return ['create-disclosure', 'user-dashboard/entities', 'disclosure/summary','entity-management/entity-details' , 'user-dashboard/disclosures'].some(ele => this._router.url.includes(ele))
  }

  ngOnDestroy(): void {
    subscriptionHandler(this.$subscriptions);
  }
   clearSfiNavBarStyle() {
    document.getElementById('COI_SCROLL').style.removeProperty('overflow');
  }

  updateRelationship(event) {
    this.updateRelationshipDetails = event;
  }


  scrollPosition(event) {
    if (event) {
      scrollIntoView('focusQuestionnaire');
    }
  }

	fullPageNavigationLeavePage() {
		this.entityDetailService.isRelationshipQuestionnaireChanged = false;
    this.entityDetailService.isAdditionalDetailsChanged = false;
    this.entityDetailService.unSavedSections = [];
    this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
		this.closeUnsavedChangesModal();
	}

  closeUnsavedChangesModal() {
    hideModal('hiddenUnsavedChanges');
  }

	getSfiEntityDetails() {
		this.$subscriptions.push(this.entityDetailService.getCoiEntityDetails(this.entityId).subscribe((res: any) => {
			this.entityDetails = res.coiEntity;
		}, _error => {
			this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
		}));
	}

	showQuestionnaireLeaveConfirmationModal(event) {
		if (event) {
      this.currentRelationshipDetails = event.details;
      this.questionnaireSection = this.entityDetailService.unSavedSections.find(ele => ele.includes('Relationship Questionnaire'));
      openModal('questionnaireUnsavedChanges');
		}
	}

  questionnaireChangeModalLeaveTab() {
    this.entityDetailService.isRelationshipQuestionnaireChanged = false;
    let index = this.entityDetailService.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
    if (index >= 0) {
      this.entityDetailService.unSavedSections.splice(index, 1);    
    }
    this.entityDetailService.$relationshipTabSwitch.next(this.currentRelationshipDetails);
    this.isSwitchCurrentTab = true;
    hideModal('questionnaireUnsavedChanges');
  }

	emittedDeletedRelationship(event) {
		this.deleteRelationshipEvent = event;
	}

  closeSlider(event) {
    this.closeAction.emit(false);
  }

  cancelConcurrency() {
    this.entityDetailService.concurrentUpdateAction = '';
}

}
