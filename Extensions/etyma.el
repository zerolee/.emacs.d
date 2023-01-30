;;;  etyma.el ---  词根词缀学习 -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; 用来学习词根词缀的
;;; Code:
(require 'cl-lib)
(require 'sqlite)

;;; 若是没有数据库则创建数据库，并且将其绑定到 etyma-db 上
(defvar etyma-db (sqlite-open "~/.emacs.d/etyma"))

;;; 创建词根表
(defun etyma-create-table ()
  "创建词根表"
;;; 创建词根表 主键，词根，词根释义，拥有词根的单词，单词释义
  (sqlite-execute etyma-db "create table etyma
(id integer PRIMARY KEY,
word character,
explain text,
example character,
example_explain text)"))
;;; 添加字段
;; (sqlite-execute etyma-db "alter table etyma add column word_list integer")

(when (< (caar (sqlite-execute etyma-db "select count(*) from sqlite_master where type='table' and name='etyma'")) 1)
  (etyma-create-table))

;;; 获取最大的 id
(defvar etyma-next-id (1+ (or (caar (sqlite-select etyma-db "select max(id) from etyma")) 0)))

;;; 删除词根表
(defun etyma-drop-table ()
  "删除词根表"
  (sqlite-execute etyma-db "drop table etyma"))

;;; 添加词根
(defun etyma-insert (word explain example example_explain word_list &optional id)
  "添加词根"
  (sqlite-execute
   etyma-db
   (format "insert into etyma values (%s, '%s', '%s', '%s', '%s', '%s')"
           (or id etyma-next-id) word explain example example_explain word_list))
  (setf etyma-next-id (1+ (caar (sqlite-select etyma-db "select max(id) from etyma")))))



;; (etyma-insert "opt" "选择" "option" "n. 选择")
;; (etyma-update 16 "id=17")
;; (etyma-delete "id=11")
;; (etyma-display t)

;;; 删除词根
;;; 用的时候多多注意
(defun etyma-delete (where)
  "删除词根"
  (sqlite-execute etyma-db (format "delete from etyma where %s" where))
  (setf etyma-next-id (1+ (or (caar (sqlite-select etyma-db "select max(id) from etyma")) 0))))

;;; 更改词根
(defun etyma-update (id sets)
  "更改某一个词根"
  (sqlite-execute etyma-db (format "UPDATE etyma SET %s where id='%s'" sets id)))

;;; 查询词根
(cl-defun etyma-select (where &optional (order "id"))
  "查询词根"
  (sqlite-select etyma-db (format "select * from etyma where %s order by %s" where order)))

(defvar etyma-random-id nil)
;;; 使用
(defun etyma-display (&optional whole)
  "展示数据库中的数据，指定数量， t 表示全部，否则任意一个."
  (if whole
      (progn
        (if (eq whole t)
            (setf whole "1=1")
          (let ((range (string-split whole "-")))
            (if (= (length range) 1)
                (setf whole (format "id=%s" (car range)))
              (setf whole (format "id between %s and %s" (car range) (cadr range))))))
        (goto-char (point-max))
        (dolist (etymas (etyma-select whole))
          (dolist (etyma etymas)
            (insert (format "| %s " etyma)))
          (insert "\n")))
;;; 随机抽取一个值
    (if etyma-random-id
        (let ((id etyma-random-id))
          (setf etyma-random-id nil)
          (car (etyma-select (format "id=%s" id))))
      (setf etyma-random-id (1+ (random (1- etyma-next-id))))
      (take 2 (car (etyma-select (format "id=%s" etyma-random-id)))))))

;;; 保存为 JSON 文件
(defun etyma-write-to-json (filename)
  "将 sqlite 中的数据以 json 的形式存储在文件中."
  (with-temp-buffer
    (insert "[")
    (dolist (etyma (etyma-select "1=1"))
      (insert
       (format "{\"id\":\"%s\",\"name\":\"%s\",\"value\":\"%s\",\"example\":{\"name\":\"%s\",\"value\":\"%s\"},\"belong\":\"%s\"},"
               (cl-first etyma)
               (cl-second etyma)
               (string-replace "\"" "'" (cl-third etyma))
               (cl-fourth etyma)
               (string-replace "\"" "'" (cl-fifth etyma))
               (cl-sixth etyma))))
    (when (char-equal (char-before) ?,)
      (delete-char -1))
    (insert "]")
    (write-region nil nil filename)))

(provide 'etyma)
;;; etyma.el ends here
