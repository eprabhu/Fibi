import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { slideHorizontal } from '../../../../../../../../fibi/src/app/common/utilities/animations';
import { CoiSummaryEventsAndStoreService } from '../../../coi-summary-events-and-store.service';

@Component({
  selector: 'app-add-comment-slider',
  templateUrl: './add-comment-slider.component.html',
  styleUrls: ['./add-comment-slider.component.scss'],
  animations: [slideHorizontal]
})
export class AddCommentSliderComponent implements OnInit {

  @Input() isOpenSlider = true;
  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

  log = [1,2,3];

  @ViewChild('addCommentOverlay', { static: true }) addCommentOverlay: ElementRef;

  constructor(public dataStoreService: CoiSummaryEventsAndStoreService) { }

  ngOnInit() {
    this.showTaskNavBar();
  }

  showTaskNavBar() {
    if (this.isOpenSlider) {
      this.addCommentOverlay.nativeElement.style.display = 'block';
      document.documentElement.classList.add('cdk-global-scrollblock');
    } else {
      this.addCommentOverlay.nativeElement.style.display = 'none';
      document.documentElement.classList.remove('cdk-global-scrollblock');
    }
  }

  hideSfiNavBar() {
        this.addCommentOverlay.nativeElement.style.display = 'block';
        this.isOpenSlider = false;
        document.documentElement.classList.remove('cdk-global-scrollblock');
        setTimeout(() => {
          this.closePage.emit(false);
        }, 1500)
    }

}
