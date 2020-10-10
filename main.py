#!/usr/bin/env python

from pyswip import Prolog
import pygame
import pygame.transform
import pygame_menu
import time
# import logging

# mancala game vars
# pits_number = 6
# pieces_in_pit = 4
# board = [[pieces_in_pit] * pits_number, [pieces_in_pit] * pits_number]
# board = [[0,0,0,0,0,1], [1,0,0,0,0,1]]
# board = [[0, 4, 0, 0, 0, 1], [0, 0, 0, 0, 0, 0]]
# current_player_number = 1
# player1 = {"name": "maayan", "func": "", "colour": (0, 0, 180), "score": 0, "extra_args": [10]}
# player2 = {"name": "alphabeta_9", "func": "alphabeta_ai", "colour": (0, 128, 0), "score": 0, "extra_args": [9]}
# players = [player1, player2]

# pygame init
pygame.init()
# screen_width = 1200
# screen_height = 500
# menu_width = screen_width
# menu_height = 50
# result_pit_width = int(screen_width / 8)
# pits_surface_width = screen_width - 2 * result_pit_width
# pits_surface_height = int(screen_height - 2 * menu_height)
# pits_board = [row[:] for row in board]
# pit_surface_width = int(pits_surface_width / pits_number)
# pit_surface_height = int(pits_surface_height / 2)

# prolog init
prolog = Prolog()
prolog.consult("mancala.pl")


class MancalaGame:
    def __init__(self, player1=None, player2=None, pits_number=6, pieces_in_pit=4, screen_width=1200, screen_height=500, menu_height=50):
        self.player1 = player1
        self.player2 = player2
        self.player1_score = 0
        self.player2_score = 0
        self.players = [player1, player2]
        self.current_player_number = 1
        self.board = [[pieces_in_pit] * pits_number, [pieces_in_pit] * pits_number]
        self.screen_width = screen_width
        self.screen_height = screen_height
        self.screen = pygame.display.set_mode([self.screen_width, self.screen_height])
        self.menu_width = screen_width
        self.menu_height = menu_height
        self.result_pit_width = int(screen_width / 8)
        self.pits_surface_width = screen_width - 2 * self.result_pit_width
        self.pits_surface_height = int(screen_height - 2 * menu_height)
        self.pits_board = [row[:] for row in self.board]
        self.pit_surface_width = int(self.pits_surface_width / pits_number)
        self.pit_surface_height = int(self.pits_surface_height / 2)

    def start_menu(self):
        self.player1 = MancalaPlayer()
        self.player2 = MancalaPlayer()

        def start_the_game():
            menu.disable()

        menu = pygame_menu.Menu(self.screen_height, self.screen_width, 'Welcome', theme=pygame_menu.themes.THEME_BLUE)
        menu.add_text_input('player1 name:', default='Maayan', onchange=self.player1.set_name)
        menu.add_selector('player1 func:', [('human', ''), ('alphabeta', 'alphabeta_ai'), ('random', 'random_ai')], onchange=self.player1.set_func)
        # if "alphabeta" in self.player1.func:
        menu.add_selector('player1 alphabeta level:', [('Very easy', 1), ('Easy', 3), ('Medium', 5), ('Hard', 7), ('Very hard', 9)], onchange=self.player1.set_alphabeta_level) #onreturn
        menu.add_color_input('player1 color: ', color_type='rgb', default=(0, 0, 180), onchange=self.player1.set_colour, font_size=18)

        menu.add_text_input('player2 name:', default='Computer', onchange=self.player2.set_name)
        menu.add_selector('player2 func:', [('human', ''), ('alphabeta', 'alphabeta_ai'), ('random', 'random_ai')], onchange=self.player2.set_func)
        # if "alphabeta" in self.player2.func:
        menu.add_selector('player2 alphabeta level:', [('Very easy', 1), ('Easy', 3), ('Medium', 5), ('Hard', 7), ('Very hard', 9)], onchange=self.player2.set_alphabeta_level)  # onreturn
        menu.add_color_input('player2 color: ', color_type='rgb', default=(0, 0, 180), onchange=self.player2.set_colour, font_size=18)
        menu.add_button('Play', start_the_game)
        # menu.add_button('reset settings', pygame_menu.events.RESET)
        menu.add_button('Quit', pygame_menu.events.EXIT)

        menu.mainloop(self.screen)

    def play(self):
        if not self.player1 or not self.player2:
            self.start_menu()
            self.players = [self.player1, self.player2]
        # Fill the background with white
        self.screen.fill((255, 255, 255))

        # menu(screen)

        pits_surface = pygame.Surface((self.pits_surface_width, self.pits_surface_height))
        up_menu = pygame.Surface((self.menu_width, self.menu_height))
        up_menu.fill((200, 200, 200))
        down_menu = pygame.Surface((self.menu_width, self.menu_height))
        down_menu.fill((200, 200, 200))
        players_name_font = pygame.font.SysFont("monospace", 30)

        self.update_pits_board()

        # Run until the user asks to quit
        running = True
        while running:
            self.screen.fill((255, 255, 255))
            current_player = self.players[self.current_player_number - 1]

            # Did the user click the window close button?
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

                # handle mouse hover
                if event.type == pygame.MOUSEMOTION and not current_player.func:
                    pos = pygame.mouse.get_pos()
                    sprites = self.pits_board[self.current_player_number - 1]

                    # get a list of all sprites that are under the mouse cursor
                    hovered_sprites = [s for s in sprites if s.rect.collidepoint(pos)]
                    if len(hovered_sprites) > 0:
                        pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    else:
                        pygame.mouse.set_cursor(*pygame.cursors.arrow)

                # handle left click event
                if event.type == pygame.MOUSEBUTTONUP and event.button == 1 and not current_player.func:
                    pos = pygame.mouse.get_pos()
                    sprites = self.pits_board[self.current_player_number - 1]

                    # get a list of all sprites that are under the mouse cursor
                    clicked_sprites = [s for s in sprites if s.rect.collidepoint(pos)]
                    if len(clicked_sprites) > 0:
                        clicked_sprites[0].click()

            if current_player.func:
                pygame.mouse.set_cursor(*pygame.cursors.arrow)
                start_time = time.time()

                if current_player.extra_args:
                    pit_index = self.prolog_ai_move(current_player.func, current_player.extra_args)
                else:
                    pit_index = self.prolog_ai_move(current_player.func)
                self.do_move(pit_index)
                exec_time = time.time() - start_time
                if exec_time < 0:
                    time.sleep(0 - exec_time)

            for j in range(len(self.board[0])):
                pits_surface.blit(self.pits_board[0][j].surface,
                                  (self.pit_surface_width * (len(self.board[0]) - j - 1), self.pit_surface_height * 0))

            for j in range(len(self.board[1])):
                pits_surface.blit(self.pits_board[1][j].surface, (self.pit_surface_width * j, self.pit_surface_height * 1))

            self.screen.blit(pits_surface, (self.result_pit_width, self.menu_height))

            player1_result_pit = PitSurface(self.player1_score, self.result_pit_width, self.pit_surface_height * 2,
                                            colour=self.player1.colour).surface
            player2_result_pit = PitSurface(self.player2_score, self.result_pit_width, self.pit_surface_height * 2,
                                            colour=self.player2.colour).surface
            self.screen.blit(player1_result_pit, (0, self.menu_height))
            self.screen.blit(player2_result_pit, (self.screen_width - self.result_pit_width, self.menu_height))

            if self.current_player_number == 1:
                label = players_name_font.render(self.player1.name, 1, self.player1.colour)
                label_rect = label.get_rect(center=(int(self.menu_width / 2), int(self.menu_height / 2)))
                up_menu.blit(label, label_rect)
                down_menu.fill((200, 200, 200))
            elif self.current_player_number == 2:
                label = players_name_font.render(self.player2.name, 1, self.player2.colour)
                label_rect = label.get_rect(center=(int(self.menu_width / 2), int(self.menu_height / 2)))
                down_menu.blit(label, label_rect)
                up_menu.fill((200, 200, 200))
            self.screen.blit(up_menu, (0, 0))
            self.screen.blit(down_menu, (0, self.screen_height - self.menu_height))

            winner = self.check_winner()
            if winner:
                try:
                    print(f"The winner is {self.players[winner - 1].name}!!!!")
                except IndexError:
                    print("The game end with tie")
                running = False

            # Flip the display
            pygame.display.flip()

        print("goodbye")
        time.sleep(3)
        # Done! Time to quit.
        pygame.quit()

    def do_move(self, pit_index):
        prolog_query = list(prolog.query(
            f"do_move({self.board}, {self.current_player_number}, {self.player1_score}, {self.player2_score},{pit_index}, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)"))
        if len(prolog_query) > 0:
            result = prolog_query[0]
            self.board = result["NewBoard"]
            self.player1_score = result["NewPlayer1Score"]
            self.player2_score = result["NewPlayer2Score"]
            print(f"board: {self.board}, {self.player1.name} score: {self.player1_score}, {self.player2.name} score: {self.player2_score} turn: {self.current_player_number}")
            self.current_player_number = result["NextPlayer"]
            self.update_pits_board()
            if sum(self.board[self.current_player_number - 1]) == 0:
                current_player_number = (self.current_player_number % 2) + 1

    def update_pits_board(self):
        for j in range(len(self.board[0])):
            pit_surface = PitSurface(self.board[0][j], self.pit_surface_width, self.pit_surface_height, j, self.player1.colour, do_move=self.do_move)
            pit_surface.rect.move_ip(self.result_pit_width + self.pit_surface_width * (len(self.board[0]) - j - 1), self.pit_surface_height * 0)
            self.pits_board[0][j] = pit_surface

        for j in range(len(self.board[1])):
            pit_surface = PitSurface(self.board[1][j], self.pit_surface_width, self.pit_surface_height, j, self.player2.colour, do_move=self.do_move)
            pit_surface.rect.move_ip(self.result_pit_width + self.pit_surface_width * j, self.pit_surface_height * 1)
            self.pits_board[1][j] = pit_surface

    def check_winner(self):
        if sum(self.board[0]) + sum(self.board[1]) == 0:
            if self.player1_score > self.player2_score:
                return 1
            elif self.player1_score < self.player2_score:
                return 2
            else:
                return 3
        else:
            return 0

    def prolog_ai_move(self, prolog_func, extra_args=[]):
        if extra_args:
            query_string = f"{prolog_func}({self.board}, {self.current_player_number}, Pit, {str(extra_args)[1:-1]})"
        else:
            query_string = f"{prolog_func}({self.board}, {self.current_player_number}, Pit)"
        prolog_query = list(prolog.query(query_string))
        pit = prolog_query[0]["Pit"]
        if isinstance(pit, int):
            return pit
        else:
            raise ValueError


class MancalaPlayer:
    def __init__(self, name="Maayan", colour=(0, 0, 180), func="", extra_args=[]):
        self.name = name
        self.func = func
        self.colour = colour
        self.extra_args = extra_args

    def set_func(self, func_label, func_name):
        self.func = func_name

    def set_name(self, name):
        self.name = name

    def set_colour(self, colour):
        self.colour = colour

    def set_extra_args(self, extra_args):
        self.extra_args = extra_args

    def set_alphabeta_level(self, label, depth):
        self.set_extra_args([depth])


class PitSurface(pygame.sprite.Sprite):
    def __init__(self, pieces_amount, width, height, index=-1, colour=(0, 0, 0), do_move=None):
        super(PitSurface, self).__init__()
        self.pieces_amount = pieces_amount
        self.index = index + 1
        image = pygame.image.load("square-the-circle.jpg").convert()
        self.surface = pygame.transform.scale(image, (width, height))
        self.rect = self.surface.get_rect()
        self.font = pygame.font.SysFont("monospace", 30)
        self.label = self.font.render(str(self.pieces_amount), 1, colour)
        label_rect = self.label.get_rect(center=(int(width / 2), int(height / 2)))
        self.surface.blit(self.label, label_rect)
        self.do_move = do_move

    def click(self):
        if self.index >= 0:
            self.do_move(self.index)
            # update_pits_board()


if __name__ == '__main__':
    # player1 = MancalaPlayer("maayan", (0, 0, 180), func="")
    # player2 = MancalaPlayer("alphabeta_3", (0, 128, 0), func="alphabeta_ai", extra_args=[3])
    # game = MancalaGame(player1, player2)

    game = MancalaGame()
    game.play()
