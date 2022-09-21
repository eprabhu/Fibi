import { Injectable } from '@angular/core';
import { Observable, Subject } from 'rxjs';
import { io } from 'socket.io-client';
import { CommonService } from './common.service';

declare var $: any;

class LockItem {
	groupId: string;
	currentUser: string;
	tabId: string;
	currentUserId: number;
	activeUsers: Array<User> = [];
	lockedItemTitle ?: string;
	createTimeStamp: number;
	isLockManuallyReleased ?: boolean;
}

interface User {
	id: string;
	name: string;
}

@Injectable()
export class WebSocketService {

	private socket: any = {};
	private promiseResolve: any;
	currentLockedModule = new LockItem();
	selfMessage$ = new Subject();
	lockFail$ = new Subject();
	isModuleLockAvailable: 'Available' | 'NotAvailAble' | 'NotTriggered' = 'NotTriggered' ;
	currentModuleId: number;
	currentModuleDescription: string;
	isShowChatWindow = false;
	private isServerAvailble = true;
	private isLockActivated = false;
	private failedReleaseLockDetails = new LockItem();
	lockedList: Array<LockItem>  = [];
	isLockReleasedManually = false;


	constructor(private _commonService: CommonService) {
		if (this._commonService.isEnableSocket) {
			this.socket = io(this._commonService.socketUrl , {'transports': ['websocket'], 'timeout': 3000});
			if (this._commonService.isEnableLock) {
				this.getModuleLockResponse();
				this.alreadyLockedEvent();
				this.lockReleaseEvent();
				this.getMessage();
				this.errorHandler();
				this.getLockListForUser();
			}
		}
	}

	listenToSocketEvents(eventName: string): Observable<any> {
		return new Observable((subscriber) => {
			this.socket.on(eventName, (data: any) => {
				subscriber.next(data);
			});
		});
	}

	errorHandler(): void {
		this.socket.on('connect_error', (data: any) => {
			this.isServerAvailble = false;
			if (this.promiseResolve) {
				this.isModuleLockAvailable = 'Available';
				this.promiseResolve(false);
				this.promiseResolve = null;
			}
		});

		this.socket.on('connect', (data) => {
			this.isServerAvailble = true;
			if (this.currentModuleId && this.currentModuleDescription && this._commonService.isEnableLock && this.isLockActivated) {
				this.getLockForModule(this.currentModuleDescription, this.currentModuleId);
			}
			if (this.failedReleaseLockDetails.groupId) {
				setTimeout(() => {
				const DATA =  this.getLockData(this.failedReleaseLockDetails.groupId);
				this.emit('releaseModuleLock', DATA);
				this.failedReleaseLockDetails = new LockItem();
				}, 5000);
			}
        });

        this.socket.on('disconnect', (data) => {
           this.isServerAvailble = false;
        });
	}

	private emit(eventName: string, data: any): void {
		if (this.isServerAvailble) {
			this.socket.emit(eventName, data);
		}
	}

	getLockForModule(moduleName: string, moduleItemKey: string | number , LockedItemTitle = ''): void {
		this.isLockActivated = true;
		if (moduleName && moduleItemKey && this._commonService.isEnableLock && this.isServerAvailble) {
			const DATA = this.getLockData(moduleName + '#' + moduleItemKey, LockedItemTitle);
			if (this.isModuleLockAvailable === 'Available' || this.isModuleLockAvailable === 'NotTriggered') {
				this.emit('getModuleLock', DATA);
				this.setCurrentActiveDocument(DATA);
				this.getLockListForUser();
			} else {
				this.emit('addToActiveUser', DATA);
			}
		}
	}

	releaseCurrentModuleLock(): void {
		if (this.currentLockedModule && this.currentLockedModule.groupId  && this.isServerAvailble) {
			const DATA = this.getLockData();
			this.emit('releaseModuleLock', DATA);
		} else if ( !this.isServerAvailble && !this.failedReleaseLockDetails.groupId &&
			this.currentLockedModule && this.currentLockedModule.groupId) {
			this.failedReleaseLockDetails = this.getLockData();
		}
		this.isLockActivated = false;
		this.resetCurrentActiveDocument();
		this.setChatWindowStatus(false);
		this.currentLockedModule = new LockItem();
		this.isModuleLockAvailable = 'NotTriggered';
		this.currentModuleDescription = '';
		this.selfMessage$.next('clear');
	}

	private getLockData(id = null, lockedItemTitle = ''): LockItem {
		const DATA = new LockItem();
		DATA.groupId = id || this.currentLockedModule.groupId;
		DATA.currentUser = this._commonService.getCurrentUserDetail('fullName');
		DATA.currentUserId = this._commonService.getCurrentUserDetail('personID');
		DATA.tabId = this._commonService.tabId;
		DATA.lockedItemTitle = lockedItemTitle;
		return DATA;
	}

	sendMessage(message: string): void {
		const data: any = {};
		data.groupId = this.currentLockedModule.groupId;
		data.user = this._commonService.getCurrentUserDetail('fullName');
		data.message = message;
		this.emit('sendMessage', data);
	}

	private setCurrentActiveDocument(data: LockItem) {
		this.currentLockedModule = data;
	}

	private resetCurrentActiveDocument(): void {
		this.currentLockedModule = new LockItem();
	}

	isModuleLocked(moduleName: string, moduleItemKey: number): Promise<boolean> {
		this.currentModuleDescription = moduleName;
		this.currentModuleId = moduleItemKey;
		return new Promise((resolve, reject) => {
			if (this._commonService.isEnableLock && this.isServerAvailble) {
				this.checkModuleLocked(moduleName + '#' + moduleItemKey);
				this.promiseResolve = resolve;
			} else {
				this.isModuleLockAvailable = 'Available';
				resolve(false);
			}
		});
	}

	private checkModuleLocked(moduleItem: string): void {
		this.emit('isModuleLocked', moduleItem);
	}

	private getModuleLockResponse(): void {
		this.listenToSocketEvents('isModuleLockedResponse').subscribe((data: LockItem) => {
			if (this.promiseResolve) {
				this.setCurrentActiveDocument(data);
				const lockStatus =  this.checkLockStatus(data);
				this.isModuleLockAvailable = lockStatus ? 'Available' : 'NotAvailAble';
				this.promiseResolve(!lockStatus);
				this.promiseResolve = null;
				this.isLockActivated = true;
			}
		});
	}

	private checkLockStatus(data: LockItem): boolean {
		return !data || data && this._commonService.getCurrentUserDetail('personID') === data.currentUserId &&
			this._commonService.tabId === data.tabId;
	}

	private alreadyLockedEvent(): void {
		this.listenToSocketEvents('alreadyLocked').subscribe((data: LockItem) => {
			this.currentLockedModule = data;
			this.isModuleLockAvailable = 'NotAvailAble';
			this.lockFail$.next(true);
			this.showModal();
		});
	}

	private lockReleaseEvent(): void {
		this.listenToSocketEvents('leftEvent').subscribe((data: LockItem) => {
			if (!this.isLockOwner(data)) {
				const CONTENT = `The lock of Proposal ${this.currentModuleId} has been released.
			 Please refresh the screen to access the proposal and see the latest changes or edit the fields, if any.`;
			 this._commonService.showToast('HTTP_INFO', CONTENT, 600000);
			} else {
				this.isLockReleasedManually = data.isLockManuallyReleased ? true : false;
				this.showModal();
			}
			this.setChatWindowStatus(false);
			this.removeModuleFromLockList(data);
			this.currentModuleId = null;
			this.currentModuleDescription = null;
		});
	}

	private getMessage(): void {
		this.listenToSocketEvents('receiveMessage').subscribe((data: string) => {
			this.setChatWindowStatus(true);
			this.selfMessage$.next(data);
		});
	}

	setChatWindowStatus(status: boolean): void {
		this.isShowChatWindow = status;
	}

	showModal(): void {
		$('#LockModal').modal('show');
	}

	intiateLocktListRequest(): void {
		this.emit('getLockList' , {currentUserId:  this._commonService.getCurrentUserDetail('personID') });
	}

	private getLockListForUser(): void {
		this.listenToSocketEvents('getLockListResponse').subscribe((data: Array<LockItem>) => {
			this.lockedList = data;
		});
	}

	releaseLockOnDemand(lockDetail): void {
		this.emit('removeLockForcefully', lockDetail);
	}

	isLockOwner(socketData): boolean {
		return (
		  socketData.currentUserId ===  this._commonService.getCurrentUserDetail('personID') &&
		  socketData.tabId === this._commonService.tabId
		);
	}

	removeModuleFromLockList(releasedLockDetail: LockItem): void {
		const INDEX = this.lockedList.findIndex(L => L.groupId === releasedLockDetail.groupId);
		if (INDEX > -1) {
			this.lockedList.splice(INDEX, 1);
		}
	}

}
