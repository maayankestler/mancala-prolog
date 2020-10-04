#!/usr/bin/env python

from pyswip import Prolog
import pygame
import pygame.transform
import time
# import logging

# mancala game vars
pits_number = 6
pieces_in_pit = 4
board = [[pieces_in_pit] * pits_number, [pieces_in_pit] * pits_number]
current_player_number = 1
player1 = {"name": "Maayan", "func": "", "colour": (0, 0, 180), "score": 0}
player2 = {"name": "ComputerAI", "func": "", "colour": (0, 128, 0), "score": 0}
players = [player1, player2]

# pygame init
pygame.init()
screen_width = 1200
screen_height = 500
menu_width = screen_width
menu_height = 50
result_pit_width = int(screen_width / 8)
pits_surface_width = screen_width - 2 * result_pit_width
pits_surface_height = int(screen_height - 2 * menu_height)
pits_board = [row[:] for row in board]
pit_surface_width = int(pits_surface_width / pits_number)
pit_surface_height = int(pits_surface_height / 2)

# prolog init
prolog = Prolog()
prolog.consult("mancala.pl")


def do_move(pit_index):
    global board, current_player_number, player1, player2
    prolog_query = list(prolog.query(f"do_move({board}, {current_player_number}, {player1['score']}, {player2['score']},{pit_index}, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)"))
    if len(prolog_query) > 0:
        result = prolog_query[0]
        board = result["NewBoard"]
        player1["score"] = result["NewPlayer1Score"]
        player2["score"] = result["NewPlayer2Score"]
        current_player_number = result["NextPlayer"]
        update_pits_board()
        if sum(board[current_player_number - 1]) == 0:
            current_player_number = (current_player_number % 2) + 1


def update_pits_board():
    for j in range(len(board[0])):
        pit_surface = PitSurface(board[0][j], pit_surface_width, pit_surface_height, j, player1["colour"])
        pit_surface.rect.move_ip(result_pit_width + pit_surface_width * (len(board[0]) - j - 1), pit_surface_height * 0)
        pits_board[0][j] = pit_surface

    for j in range(len(board[1])):
        pit_surface = PitSurface(board[1][j], pit_surface_width, pit_surface_height, j, player2["colour"])
        pit_surface.rect.move_ip(result_pit_width + pit_surface_width * j, pit_surface_height * 1)
        pits_board[1][j] = pit_surface


class PitSurface(pygame.sprite.Sprite):
    def __init__(self, pieces_amount, width, height, index=-1, colour=(0, 0, 0)):
        super(PitSurface, self).__init__()
        self.pieces_amount = pieces_amount
        self.index = index + 1
        image = pygame.image.load("square-the-circle.jpg").convert()
        self.surface = pygame.transform.scale(image, (width, height))
        self.rect = self.surface.get_rect()
        self.font = pygame.font.SysFont("monospace", 30)
        self.label = self.font.render(str(self.pieces_amount), 1, colour)
        label_rect = self.label.get_rect(center=(width / 2, height / 2))
        self.surface.blit(self.label, label_rect)

    def click(self):
        if self.index >= 0:
            do_move(self.index)
            # update_pits_board()


# class PlayerSurface(pygame.sprite.Sprite):
#     def __init__(self, name, width, height, colour=(0, 0, 0)):
#         super(PitSurface, self).__init__()
#         self.name = name
#         self.surface = pygame.transform.scale(image, (width, height))
#         self.rect = self.surface.get_rect()
#         self.font = pygame.font.SysFont("monospace", 30)
#         self.label = self.font.render(str(self.pieces_amount), 1, colour)
#         label_rect = self.label.get_rect(center=(width / 2, height / 2))
#         self.surface.blit(self.label, label_rect)


def check_winner():
    if sum(board[0]) + sum(board[1]) == 0:
        if player1["score"] > player2["score"]:
            return 1
        elif player1["score"] < player2["score"]:
            return 2
        else:
            return 3
    else:
        return 0


def prolog_ai_move(prolog_func, *args):
    if len(args) > 0:
        query_string = f"{prolog_func}({board}, {current_player_number}, Pit, {str(args)[1:-1]})"
    else:
        query_string = f"{prolog_func}({board}, {current_player_number}, Pit)"
    prolog_query = list(prolog.query(query_string))
    return prolog_query[0]["Pit"]


if __name__ == '__main__':
    # Set up the drawing window
    screen = pygame.display.set_mode([screen_width, screen_height])
    # Fill the background with white
    screen.fill((255, 255, 255))

    pits_surface = pygame.Surface((pits_surface_width, pits_surface_height))
    up_menu = pygame.Surface((menu_width, menu_height))
    up_menu.fill((200, 200, 200))
    down_menu = pygame.Surface((menu_width, menu_height))
    down_menu.fill((200, 200, 200))
    players_name_font = pygame.font.SysFont("monospace", 30)

    update_pits_board()

    # Run until the user asks to quit
    running = True
    while running:
        current_player = players[current_player_number - 1]

        # Did the user click the window close button?
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

            # handle mouse hover
            if event.type == pygame.MOUSEMOTION and not current_player["func"]:
                pos = pygame.mouse.get_pos()
                sprites = pits_board[current_player_number - 1]

                # get a list of all sprites that are under the mouse cursor
                hovered_sprites = [s for s in sprites if s.rect.collidepoint(pos)]
                if len(hovered_sprites) > 0:
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                else:
                    pygame.mouse.set_cursor(*pygame.cursors.arrow)

            # handle left click event
            if event.type == pygame.MOUSEBUTTONUP and event.button == 1 and not current_player["func"]:
                pos = pygame.mouse.get_pos()
                sprites = pits_board[current_player_number - 1]

                # get a list of all sprites that are under the mouse cursor
                clicked_sprites = [s for s in sprites if s.rect.collidepoint(pos)]
                if len(clicked_sprites) > 0:
                    clicked_sprites[0].click()

        for j in range(len(board[0])):
            pits_surface.blit(pits_board[0][j].surface, (pit_surface_width * (len(board[0]) - j - 1), pit_surface_height * 0))

        for j in range(len(board[1])):
            pits_surface.blit(pits_board[1][j].surface, (pit_surface_width * j, pit_surface_height * 1))

        screen.blit(pits_surface, (result_pit_width, menu_height))

        player1_result_pit = PitSurface(player1["score"], result_pit_width, pit_surface_height * 2, colour=player1["colour"]).surface
        player2_result_pit = PitSurface(player2["score"], result_pit_width, pit_surface_height * 2, colour=player2["colour"]).surface
        screen.blit(player1_result_pit, (0, menu_height))
        screen.blit(player2_result_pit, (screen_width - result_pit_width, menu_height))

        if current_player_number == 1:
            label = players_name_font.render(player1["name"], 1, player1["colour"])
            label_rect = label.get_rect(center=(menu_width / 2, menu_height / 2))
            up_menu.blit(label, label_rect)
            down_menu.fill((200, 200, 200))
        elif current_player_number == 2:
            label = players_name_font.render(player2["name"], 1, player2["colour"])
            label_rect = label.get_rect(center=(menu_width / 2, menu_height / 2))
            down_menu.blit(label, label_rect)
            up_menu.fill((200, 200, 200))
        screen.blit(up_menu, (0, 0))
        screen.blit(down_menu, (0, screen_height - menu_height))

        if current_player["func"]:
            pygame.mouse.set_cursor(*pygame.cursors.arrow)
            start_time = time.time()
            pit_index = prolog_ai_move(current_player["func"])
            do_move(pit_index)
            exec_time = time.time() - start_time
            if exec_time < 1:
                time.sleep(1 - exec_time)

        winner = check_winner()
        if winner:
            try:
                print(f"The winner is {players[winner - 1]['name']}!!!!")
            except IndexError:
                print("The game end with tie")
            running = False

        # Flip the display
        pygame.display.flip()

    # Done! Time to quit.
    pygame.quit()
