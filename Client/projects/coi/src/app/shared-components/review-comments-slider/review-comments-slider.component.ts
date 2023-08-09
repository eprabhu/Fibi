import { Component, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { CoiService } from '../../disclosure/services/coi.service';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { environment } from '../../../environments/environment';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';


@Component({
    selector: 'app-review-comments-slider',
    templateUrl: './review-comments-slider.component.html',
    styleUrls: ['./review-comments-slider.component.scss'],
    animations: [slideHorizontal]
})
export class ReviewCommentsSliderComponent implements OnInit, OnDestroy {

    deployMap = environment.deployUrl;
    commentTab = 'MODIFY';
    @ViewChild('commentNavOverlay', { static: true }) commentNavOverlay: ElementRef;
    isSaving = false;
    scrollHeight: number;
    $subscriptions: Subscription[] = [];
    isAddAttachment = false;
    isAddAssignee = false;
    dateTime: string;
    isReadMore = false;
    comment = '';
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();


    constructor(
        public _commonService: CommonService,
        public _coiService: CoiService
    ) { }

    ngOnInit() {
        setTimeout(() => {
            openSlider('review-comments-slider');
        });
    }

    ngOnDestroy(): void {;
        subscriptionHandler(this.$subscriptions);
    }

    openConformationModal() {
        document.getElementById('review-comments-confirmation-modal-trigger-btn').click();
    }

    validateSliderClose() {
        (this.comment) ? this.openConformationModal() : this.closeReviewSlider();
    }

    closeReviewSlider() {
        closeSlider('review-comments-slider');
        setTimeout(() => {
            this.closePage.emit();
            this._coiService.isShowCommentNavBar = false;
        }, 500);
    }

    leavePageClicked(event: boolean) {
        if (event) {
            setTimeout(() => {
                this.closeReviewSlider();
            }, 100);
        }
    }

}
