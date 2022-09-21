import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-fyi',
  templateUrl: './fyi.component.html',
  styleUrls: ['./fyi.component.css']
})
export class FyiComponent implements OnInit {

  faqArray: any[] = [];
  collapseViewMore = {};

  constructor() { }

  ngOnInit() {
    this.getFAQArray();
  }

  getFAQArray() {
   this.faqArray[0] = {'id': '1', 'question': 'Examples of Potential Research COI ?', 'answer' :'Undertaking evaluative research when the investigator or related individuals have a financial, managerial, or ownership interest in the sponsoring company'};
   this.faqArray[1] = {'id': '2', 'question': 'Perceived/Potential Risks ?', 'answer' : 'To study subjects in clinical trials . Protection of human subjects is paramount To investigators - Financial, sponsored funding and reputational risks To the institution -Financial, sponsored funding and reputational risks'};
   this.faqArray[2] = {'id': '3', 'question': 'UR COI Policy?', 'answer' : 'The UR Conflict of Commitment and Interest Policy was substantially revised in August 2012'};
   this.faqArray[3] = {'id': '4', 'question': 'UR COI Policy – Decision Making ?', 'answer' : 'Dean may establish oversight committees to review the activities of situations that are complex (e.g., monitor activities of situations that are complex (e.g., monitor conduct, ensure timely publication of research results)'};
   this.faqArray[4] = {'id': '5', 'question': 'Significant Financial Interest (SFI) Threshold ?', 'answer' : '$5,000 instead of $10,000'};
   this.faqArray[5] = {'id': '6', 'question': 'Reporting Period ?', 'answer' : 'Faculty and Investigators must submit an updated by disclosure of SFI within 30 days of discovering or acquiring the SFI'};
  }

  collapseViewMoreOption(id: number, flag: boolean): void {
    this.collapseViewMore = {};
    this.collapseViewMore[id] = !flag;
  }


}
