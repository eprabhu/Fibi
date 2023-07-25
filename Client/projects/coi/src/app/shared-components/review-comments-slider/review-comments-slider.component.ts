import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { CoiService } from '../../disclosure/services/coi.service';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { environment } from '../../../environments/environment';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';

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

    constructor(
        public _commonService: CommonService,
        public _coiService: CoiService
    ) { }

    ngOnInit() {
        this.showCommentNavBar();
    }

    ngOnDestroy(): void {
        this.showCommentNavBar();
        subscriptionHandler(this.$subscriptions);
    }

    hideCommentNavBar() {
        if (!this.isSaving) {
            this._coiService.isShowCommentNavBar = false;
        }
    }

    showCommentNavBar() {
        if (this._coiService.isShowCommentNavBar) {
            this.scrollHeight = document.documentElement.scrollTop;
            document.documentElement.classList.add('cdk-global-scrollblock');
            document.documentElement.style.top = - this.scrollHeight + 'px';
        }
        else {
            document.documentElement.classList.remove('cdk-global-scrollblock');
            document.documentElement.scrollTop = this.scrollHeight;
        }
    }

}
