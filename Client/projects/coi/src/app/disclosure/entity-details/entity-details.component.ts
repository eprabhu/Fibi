import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';
import { hideModal, scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { NavigationService } from '../../common/services/navigation.service';
import { SfiService } from '../sfi/sfi.service';
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
  onDeleteTimestamp = null;
  @Output() closeAction: EventEmitter<boolean> = new EventEmitter<boolean>();

  

  constructor(public entityDetailService: EntityDetailsService, private _route: ActivatedRoute, private _router: Router,
    private _sfiService: SfiService, private _commonService: CommonService, private _navigationService: NavigationService) {
    this.clearSfiNavBarStyle();
  }

  updateRelationshipDetails: any;
  isEditMode = false;
  entityDetails = {};

  ngOnInit() {
    this.isEditMode = this._route.snapshot.queryParamMap.get('mode') === 'edit';
    this.entityId = this._route.snapshot.queryParamMap.get('personEntityId') || this.entityId;
    this.isTriggeredFromSlider = this.checkForUrl();
    this.getSfiEntityDetails();
    this.getQueryParams();
  }

  getQueryParams() {
    this.$subscriptions.push(this._route.queryParams.subscribe(params => {
      this.isEditMode = params['mode'] === 'edit';
      this.entityId = params['personEntityId'] || this.entityId;
    }));
  }

  checkForUrl() {
   return ['create-disclosure', 'user-dashboard/entities', 'disclosure/summary','entity-management/entity-details'].some(ele => this._router.url.includes(ele))
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

	leavePage() {
		this.entityDetailService.isRelationshipQuestionnaireChanged = false;
		this.$subscriptions.push(this.entityDetailService.$relationshipTabSwitch.subscribe((res: any) => {
			res ? this.isSwitchCurrentTab = true : this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
		}))
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

	showLeaveConfirmModal(event) {
		if (event) {
			document.getElementById('hidden-unsaved-changes-button').click();
		}
	}

	emittedDeletedRelationship(event) {
		this.onDeleteTimestamp = event;
	}

  closeSlider(event) {
    this.closeAction.emit(false);
  }

}
